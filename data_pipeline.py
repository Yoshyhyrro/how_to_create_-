"""
Data Pipeline Orchestrator
Manages API fetching, validation, caching, and output
"""

import json
import pandas as pd
import logging
from typing import List, Dict, Optional
from datetime import datetime
import time
from pathlib import Path

from models import RegionalGDPRecord, RegionalGDPDataset, APIFetchResult, PipelineConfig
from fetchers import create_fetcher

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class LanguageGDPDataPipeline:
    """
    Complete ETL pipeline for Language-GDP analysis
    Handles API fetching, validation, caching, and export
    """
    
    def __init__(self, config_path: str = 'config.json'):
        """
        Initialize pipeline with configuration
        
        Args:
            config_path: Path to config.json
        """
        self.config_path = Path(config_path)
        self.config = self._load_config()
        self.records: List[RegionalGDPRecord] = []
        self.fetch_results: List[APIFetchResult] = []
        
        # Create necessary directories
        Path('./cache').mkdir(exist_ok=True)
        Path('./data/processed').mkdir(parents=True, exist_ok=True)
    
    def _load_config(self) -> Dict:
        """Load configuration from JSON file"""
        try:
            with open(self.config_path, 'r') as f:
                return json.load(f)
        except FileNotFoundError:
            logger.error(f"Config file not found: {self.config_path}")
            raise
    
    def run(self, sources: Optional[List[str]] = None, use_cache: bool = True) -> RegionalGDPDataset:
        """
        Execute full pipeline
        
        Args:
            sources: List of sources to fetch (default: from config)
            use_cache: Whether to use cached data
        
        Returns:
            Validated RegionalGDPDataset
        """
        logger.info("="*60)
        logger.info("STARTING DATA PIPELINE")
        logger.info("="*60)
        
        if sources is None:
            sources = ['world_bank']  # Start with World Bank only
        
        # Phase 1: Fetch data
        self._fetch_from_sources(sources, use_cache)
        
        # Phase 2: Validate
        dataset = self._validate_and_combine()
        
        # Phase 3: Export
        self._export_dataset(dataset)
        
        logger.info("="*60)
        logger.info("PIPELINE COMPLETE")
        logger.info(f"Total records: {len(self.records)}")
        logger.info("="*60)
        
        return dataset
    
    def _fetch_from_sources(self, sources: List[str], use_cache: bool):
        """Fetch data from specified sources"""
        logger.info(f"\nPhase 1: FETCHING from {sources}")
        
        for source in sources:
            logger.info(f"\n--- {source.upper()} ---")
            
            try:
                start_time = time.time()
                
                # Create fetcher
                fetcher = create_fetcher(source, self.config, verbose=True)
                
                # Fetch data
                records = fetcher.fetch()
                
                elapsed = time.time() - start_time
                
                # Record result
                result = APIFetchResult(
                    source=source,
                    success=len(records) > 0,
                    records_fetched=len(records),
                    errors=[],
                    warnings=[],
                    elapsed_seconds=elapsed
                )
                
                self.fetch_results.append(result)
                self.records.extend(records)
                
                logger.info(f"✓ {source}: {len(records)} records in {elapsed:.2f}s")
                
            except Exception as e:
                logger.error(f"✗ {source}: {e}")
                result = APIFetchResult(
                    source=source,
                    success=False,
                    records_fetched=0,
                    errors=[str(e)],
                    elapsed_seconds=0
                )
                self.fetch_results.append(result)
    
    def _validate_and_combine(self) -> RegionalGDPDataset:
        """Validate records and create dataset"""
        logger.info(f"\nPhase 2: VALIDATING {len(self.records)} records")
        
        # Already validated by Pydantic during fetch, but double-check
        valid_records = [r for r in self.records if isinstance(r, RegionalGDPRecord)]
        
        logger.info(f"✓ All {len(valid_records)} records valid")
        
        # Extract metadata
        languages = sorted(set(r.language for r in valid_records))
        countries = sorted(set(r.country for r in valid_records))
        years = sorted(set(r.year for r in valid_records))
        sources = sorted(set(r.source for r in valid_records))
        
        logger.info(f"  Languages: {languages}")
        logger.info(f"  Countries: {countries}")
        logger.info(f"  Years: {min(years)}-{max(years)}")
        logger.info(f"  Sources: {sources}")
        
        # Create dataset
        dataset = RegionalGDPDataset(
            records=valid_records,
            total_regions=len(valid_records),
            languages=languages,
            countries=countries,
            year_range=(min(years), max(years)),
            sources_used=sources
        )
        
        return dataset
    
    def _export_dataset(self, dataset: RegionalGDPDataset):
        """Export dataset to CSV and JSON"""
        logger.info(f"\nPhase 3: EXPORTING")
        
        # Convert to DataFrame
        df = pd.DataFrame([
            {
                'region': r.region,
                'country': r.country,
                'language': r.language,
                'gdp_billion_usd': r.gdp_billion_usd,
                'population_million': r.population_million,
                'gdp_per_capita_usd': r.gdp_per_capita_approx,
                'year': r.year,
                'source': r.source
            }
            for r in dataset.records
        ])
        
        # CSV export
        csv_path = Path(self.config['output']['path'])
        df.to_csv(csv_path, index=False)
        logger.info(f"✓ CSV exported: {csv_path}")
        
        # JSON export (metadata + schema)
        json_path = csv_path.parent / f"{csv_path.stem}_metadata.json"
        metadata = {
            'fetch_timestamp': dataset.fetch_timestamp.isoformat(),
            'total_records': dataset.total_regions,
            'languages': dataset.languages,
            'countries': dataset.countries,
            'year_range': dataset.year_range,
            'sources_used': dataset.sources_used,
            'fetch_results': [
                {
                    'source': r.source,
                    'success': r.success,
                    'records_fetched': r.records_fetched,
                    'elapsed_seconds': r.elapsed_seconds
                }
                for r in self.fetch_results
            ]
        }
        
        with open(json_path, 'w') as f:
            json.dump(metadata, f, indent=2)
        logger.info(f"✓ Metadata exported: {json_path}")
        
        # Summary stats
        logger.info(f"\nDataset Summary:")
        logger.info(f"  Shape: {df.shape}")
        logger.info(f"  Mean GDP/capita: ${df['gdp_per_capita_usd'].mean():,.0f}")
        logger.info(f"  By language:")
        for lang in dataset.languages:
            lang_df = df[df['language'] == lang]
            logger.info(f"    {lang}: {len(lang_df)} records, "
                       f"${lang_df['gdp_per_capita_usd'].mean():,.0f}/capita")
    
    def get_dataframe(self) -> pd.DataFrame:
        """Get data as pandas DataFrame"""
        if not self.records:
            raise ValueError("No records fetched. Run pipeline first.")
        
        return pd.DataFrame([
            {
                'region': r.region,
                'country': r.country,
                'language': r.language,
                'gdp_billion_usd': r.gdp_billion_usd,
                'population_million': r.population_million,
                'gdp_per_capita_usd': r.gdp_per_capita_approx,
                'year': r.year,
                'source': r.source
            }
            for r in self.records
        ])


def main():
    """Example usage"""
    
    # Initialize pipeline
    pipeline = LanguageGDPDataPipeline('config.json')
    
    # Run with World Bank only (Phase A)
    dataset = pipeline.run(sources=['world_bank'], use_cache=True)
    
    # Get DataFrame for analysis
    df = pipeline.get_dataframe()
    print("\nDataFrame shape:", df.shape)
    print("\nFirst 5 records:")
    print(df.head())
    print("\nBasic stats:")
    print(df.groupby('language')['gdp_per_capita_usd'].describe())


if __name__ == '__main__':
    main()
