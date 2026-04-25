"""
Pydantic models for Language-GDP Analysis
Handles data validation, type checking, and serialization
"""

from pydantic import BaseModel, Field, field_validator, model_validator
from typing import Optional, List
from datetime import datetime


class RegionalGDPRecord(BaseModel):
    """Single regional GDP data point with validation"""
    
    region: str = Field(
        ..., 
        min_length=1, 
        max_length=100,
        description="Region/state/prefecture/canton name"
    )
    country: str = Field(
        ...,
        description="Country name"
    )
    language: str = Field(
        ...,
        description="Language group classification"
    )
    gdp_billion_usd: float = Field(
        ..., 
        gt=0,
        description="Nominal GDP in billions of USD"
    )
    population_million: float = Field(
        ..., 
        gt=0,
        description="Population in millions"
    )
    year: int = Field(
        ..., 
        ge=1990, 
        le=2024,
        description="Reference year"
    )
    source: str = Field(
        ...,
        description="Data source API (e.g., 'world_bank', 'destatis')"
    )
    gdp_per_capita_approx: Optional[float] = Field(
        None,
        description="Approximate GDP per capita (auto-calculated)"
    )
    
    @field_validator('language')
    @classmethod
    def validate_language(cls, v):
        """Ensure language is in allowed set"""
        allowed = {'German', 'Japanese', 'Spanish'}
        if v not in allowed:
            raise ValueError(f'Language must be one of {allowed}, got {v}')
        return v
    
    @field_validator('country')
    @classmethod
    def validate_country(cls, v):
        """Ensure country is in expected set"""
        allowed = {'Germany', 'Austria', 'Switzerland', 'Japan', 'Spain', 'Mexico'}
        if v not in allowed:
            raise ValueError(f'Country must be one of {allowed}, got {v}')
        return v
    
    @model_validator(mode='after')
    def calculate_gdp_per_capita(self):
        """Auto-calculate GDP per capita"""
        self.gdp_per_capita_approx = (
            (self.gdp_billion_usd * 1e9) / (self.population_million * 1e6)
        )
        return self
    
    class Config:
        """Pydantic config"""
        str_strip_whitespace = True
        json_schema_extra = {
            "example": {
                "region": "Tokyo",
                "country": "Japan",
                "language": "Japanese",
                "gdp_billion_usd": 2100,
                "population_million": 14.0,
                "year": 2023,
                "source": "japan_cao"
            }
        }


class RegionalGDPDataset(BaseModel):
    """Complete dataset with metadata"""
    
    records: List[RegionalGDPRecord] = Field(
        ...,
        description="List of regional GDP records"
    )
    total_regions: int = Field(
        ...,
        description="Total number of regions"
    )
    languages: List[str] = Field(
        ...,
        description="Unique languages in dataset"
    )
    countries: List[str] = Field(
        ...,
        description="Unique countries in dataset"
    )
    year_range: tuple = Field(
        ...,
        description="(min_year, max_year)"
    )
    fetch_timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When data was fetched"
    )
    sources_used: List[str] = Field(
        ...,
        description="Which API sources provided data"
    )
    
    @field_validator('total_regions')
    @classmethod
    def validate_total_regions(cls, v, info):
        """Ensure total_regions matches actual records count"""
        if 'records' in info.data:
            if v != len(info.data['records']):
                raise ValueError(
                    f'total_regions ({v}) must match records count ({len(info.data["records"])})'
                )
        return v
    
    class Config:
        json_schema_extra = {
            "description": "Complete validated dataset ready for analysis"
        }


class APIFetchResult(BaseModel):
    """Result from a single API fetch operation"""
    
    source: str = Field(
        ...,
        description="API source name"
    )
    success: bool = Field(
        ...,
        description="Whether fetch succeeded"
    )
    records_fetched: int = Field(
        ...,
        ge=0,
        description="Number of records successfully fetched"
    )
    errors: List[str] = Field(
        default_factory=list,
        description="Error messages if any"
    )
    warnings: List[str] = Field(
        default_factory=list,
        description="Non-fatal warnings"
    )
    fetch_timestamp: datetime = Field(
        default_factory=datetime.utcnow
    )
    elapsed_seconds: float = Field(
        ...,
        ge=0,
        description="Fetch duration in seconds"
    )
    
    class Config:
        json_schema_extra = {
            "example": {
                "source": "world_bank",
                "success": True,
                "records_fetched": 180,
                "errors": [],
                "warnings": ["Some regions have missing 2024 data"],
                "elapsed_seconds": 2.5
            }
        }


class PipelineConfig(BaseModel):
    """Configuration for data pipeline"""
    
    sources_to_fetch: List[str] = Field(
        default=['world_bank'],
        description="Which API sources to use"
    )
    use_cache: bool = Field(
        default=True,
        description="Whether to use cached data"
    )
    validate_on_fetch: bool = Field(
        default=True,
        description="Validate data immediately after fetch"
    )
    output_format: str = Field(
        default='csv',
        description="Output format (csv, json, parquet)"
    )
    verbose: bool = Field(
        default=True,
        description="Print detailed logs"
    )
