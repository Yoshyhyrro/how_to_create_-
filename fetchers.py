"""
API Data Fetchers with Pydantic validation
Base class + World Bank implementation
"""

import json
from abc import ABC, abstractmethod
from typing import List, Dict, Optional
import requests
from datetime import datetime
import time
import logging
from models import RegionalGDPRecord, APIFetchResult


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class BaseDataFetcher(ABC):
    """Abstract base class for all API fetchers"""
    
    def __init__(self, config: Dict, verbose: bool = True):
        """
        Args:
            config: Dict with API configuration
            verbose: Whether to log detailed info
        """
        self.config = config
        self.verbose = verbose
        self.source_name = self.__class__.__name__
    
    @abstractmethod
    def fetch(self) -> List[RegionalGDPRecord]:
        """
        Fetch data from API and return validated records
        Must be implemented by subclasses
        """
        pass
    
    def _validate_and_create_records(
        self, 
        raw_data: List[Dict],
        country: str,
        language: str
    ) -> tuple[List[RegionalGDPRecord], List[str]]:
        """
        Validate raw data and create Pydantic models
        Returns (valid_records, error_messages)
        """
        valid_records = []
        errors = []
        
        for idx, item in enumerate(raw_data):
            try:
                record = RegionalGDPRecord(
                    region=item.get('region'),
                    country=country,
                    language=language,
                    gdp_billion_usd=float(item.get('gdp_billion_usd')),
                    population_million=float(item.get('population_million')),
                    year=int(item.get('year')),
                    source=self.source_name
                )
                valid_records.append(record)
            except Exception as e:
                errors.append(f"Row {idx}: {str(e)}")
                if self.verbose:
                    logger.warning(f"Validation error for {item}: {e}")
        
        return valid_records, errors
    
    def _log(self, msg: str, level: str = 'info'):
        """Conditional logging"""
        if self.verbose:
            getattr(logger, level)(msg)


class WorldBankDataFetcher(BaseDataFetcher):
    """
    Fetch data from World Bank API
    Current limitation: national-level only (Phase A)
    """
    
    def __init__(self, config: Dict, verbose: bool = True):
        super().__init__(config, verbose)
        self.base_url = config.get('world_bank', {}).get('base_url', 
                                                         'https://api.worldbank.org/v2')
        self.timeout = config.get('world_bank', {}).get('timeout', 30)
    
    def fetch(self) -> List[RegionalGDPRecord]:
        """
        Fetch GDP and population data from World Bank
        Returns list of RegionalGDPRecord (currently national level)
        """
        self._log(f"Starting World Bank API fetch...")
        
        countries_config = {
            'DEU': {'name': 'Germany', 'language': 'German'},
            'AUT': {'name': 'Austria', 'language': 'German'},
            'CHE': {'name': 'Switzerland', 'language': 'German'},
            'JPN': {'name': 'Japan', 'language': 'Japanese'},
            'ESP': {'name': 'Spain', 'language': 'Spanish'},
            'MEX': {'name': 'Mexico', 'language': 'Spanish'},
        }
        
        records = []
        
        for country_code, info in countries_config.items():
            try:
                self._log(f"  Fetching {info['name']}...")
                
                # Fetch GDP (NY.GDP.MKTP.CD = GDP nominal USD)
                gdp_data = self._fetch_indicator(country_code, 'NY.GDP.MKTP.CD')
                
                # Fetch Population (SP.POP.TOTL)
                pop_data = self._fetch_indicator(country_code, 'SP.POP.TOTL')
                
                # Merge and create records
                country_records = self._merge_indicators(
                    gdp_data, pop_data,
                    country_code, info['name'], info['language']
                )
                
                records.extend(country_records)
                self._log(f"    ✓ Got {len(country_records)} years of data")
                
            except Exception as e:
                logger.error(f"Error fetching {info['name']}: {e}")
        
        self._log(f"World Bank fetch complete: {len(records)} records")
        return records
    
    def _fetch_indicator(self, country_code: str, indicator: str) -> Dict:
        """Fetch single indicator from World Bank API"""
        url = f"{self.base_url}/country/{country_code}/indicator/{indicator}"
        params = {'format': 'json', 'per_page': 100}
        
        try:
            response = requests.get(url, params=params, timeout=self.timeout)
            response.raise_for_status()
            data = response.json()
            
            if len(data) >= 2:
                return {item['date']: item['value'] for item in data[1] if item['value']}
            return {}
        except Exception as e:
            logger.error(f"Failed to fetch {indicator} for {country_code}: {e}")
            return {}
    
    def _merge_indicators(
        self,
        gdp_data: Dict,
        pop_data: Dict,
        country_code: str,
        country_name: str,
        language: str
    ) -> List[RegionalGDPRecord]:
        """Merge GDP and population indicators into records"""
        records = []
        
        # Find overlapping years
        years = set(gdp_data.keys()) & set(pop_data.keys())
        
        for year in sorted(years, reverse=True):
            try:
                year_int = int(year)
                gdp_usd = float(gdp_data[year])
                pop = float(pop_data[year])
                
                # Only recent years
                if year_int >= 2000 and gdp_usd > 0 and pop > 0:
                    record = RegionalGDPRecord(
                        region=country_name,  # National level
                        country=country_name,
                        language=language,
                        gdp_billion_usd=gdp_usd / 1e9,
                        population_million=pop / 1e6,
                        year=year_int,
                        source='world_bank'
                    )
                    records.append(record)
            except (ValueError, TypeError) as e:
                logger.debug(f"Skipping {year}: {e}")
        
        return records


class DestatisDataFetcher(BaseDataFetcher):
    """
    Fetch data from German Federal Statistical Office (Destatis)
    GENESIS API - requires credentials
    """
    
    def fetch(self) -> List[RegionalGDPRecord]:
        """Not yet implemented - requires GENESIS API setup"""
        self._log("Destatis fetcher: Not yet implemented")
        self._log("Required: Register at destatis.de for GENESIS API access")
        return []


class StatSwissDataFetcher(BaseDataFetcher):
    """
    Fetch data from Swiss Federal Statistical Office
    Primarily file-based; REST API limited
    """
    
    def fetch(self) -> List[RegionalGDPRecord]:
        """Not yet implemented - STATSWISS lacks comprehensive REST API"""
        self._log("StatSwiss fetcher: Not yet implemented")
        return []


class JapaneseCaoDataFetcher(BaseDataFetcher):
    """
    Fetch data from Japanese Cabinet Office (内閣府)
    https://www.esri.cao.go.jp/
    """
    
    def fetch(self) -> List[RegionalGDPRecord]:
        """Not yet implemented - requires manual data sourcing"""
        self._log("Japanese CAO fetcher: Not yet implemented")
        return []


class SpanishINEDataFetcher(BaseDataFetcher):
    """
    Fetch data from Spanish National Statistics Institute (INE)
    """
    
    def fetch(self) -> List[RegionalGDPRecord]:
        """Not yet implemented"""
        self._log("Spanish INE fetcher: Not yet implemented")
        return []


def create_fetcher(source_name: str, config: Dict, verbose: bool = True) -> BaseDataFetcher:
    """
    Factory function to create appropriate fetcher
    """
    fetchers = {
        'world_bank': WorldBankDataFetcher,
        'destatis': DestatisDataFetcher,
        'statswiss': StatSwissDataFetcher,
        'japan_cao': JapaneseCaoDataFetcher,
        'ine_spain': SpanishINEDataFetcher,
    }
    
    FetcherClass = fetchers.get(source_name)
    if not FetcherClass:
        raise ValueError(f"Unknown source: {source_name}. Available: {list(fetchers.keys())}")
    
    return FetcherClass(config, verbose)
