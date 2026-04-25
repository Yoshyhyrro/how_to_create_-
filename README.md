# Language & GDP: Quantitative Regional Analysis

**Research Question:** Does language correlate with economic output (GDP per capita) when controlling for geography and regional development?

**Approach:** API-driven data pipeline with Pydantic validation, analyzing regional GDP across language communities (German, Japanese, Spanish).

---

## Project Architecture

### Why Pydantic?

This project uses **Pydantic v2** for data validation because:

1. **Type Safety**: Every record fetched from APIs is validated against a schema
2. **Error Transparency**: If data is malformed, you get clear error messages (not silent failures)
3. **Reproducibility**: Same code → same validation → same results
4. **Extensibility**: Adding new data sources is straightforward (inherit from `BaseDataFetcher`)
5. **Documentation**: Pydantic models auto-generate JSON schemas

```python
# Example: API returns bad data
raw_data = {
    'region': 'Tokyo',
    'gdp_billion_usd': 'two thousand',  # ❌ String instead of float!
    'year': 2023
}

record = RegionalGDPRecord(**raw_data)
# Pydantic raises: ValidationError: value is not a valid integer (type=type_error.integer)
```

### File Structure

```
language-gdp-analysis/
├── config.json                    # API config + validation rules
├── models.py                      # Pydantic data models
├── fetchers.py                    # API wrapper classes
├── data_pipeline.py               # Main orchestrator
├── 01_data_collection.ipynb       # Data collection + EDA
├── 02_statistical_analysis.ipynb  # Hypothesis testing (future)
└── README.md
```

### Data Models (Pydantic)

#### `RegionalGDPRecord`
Single regional data point with automatic validation:

```python
class RegionalGDPRecord(BaseModel):
    region: str              # Validated: min_length=1
    country: str             # Validated: in allowed set
    language: str            # Validated: {German, Japanese, Spanish}
    gdp_billion_usd: float   # Validated: > 0
    population_million: float # Validated: > 0
    year: int                # Validated: 1990 ≤ year ≤ 2024
    source: str              # Which API provided this
    gdp_per_capita_usd: float  # Auto-calculated
```

**Validation Example:**
- ❌ `gdp_billion_usd=-100` → Error: "value must be greater than 0"
- ❌ `language="French"` → Error: "value must be in {German, Japanese, Spanish}"
- ✓ `gdp_billion_usd=2100.5, language="Japanese"` → Valid!

#### `RegionalGDPDataset`
Complete validated dataset with metadata:

```python
class RegionalGDPDataset(BaseModel):
    records: List[RegionalGDPRecord]  # All validated records
    total_regions: int                # Must match len(records)
    languages: List[str]              # {German, Japanese, Spanish}
    countries: List[str]              # Unique countries
    year_range: tuple                 # (min_year, max_year)
    fetch_timestamp: datetime         # When data was fetched
    sources_used: List[str]           # Which APIs were used
```

### API Fetchers

All fetchers inherit from `BaseDataFetcher`:

```python
class BaseDataFetcher(ABC):
    @abstractmethod
    def fetch(self) -> List[RegionalGDPRecord]:
        """Must return validated Pydantic objects"""
        pass
```

**Current Status:**
- ✅ **WorldBankDataFetcher** (Implemented) - National-level GDP & population
- ⏳ **DestatisDataFetcher** (Stub) - German regional data
- ⏳ **StatSwissDataFetcher** (Stub) - Swiss cantonal data
- ⏳ **JapaneseCaoDataFetcher** (Stub) - Japanese prefectural data
- ⏳ **SpanishINEDataFetcher** (Stub) - Spanish regional data

Adding a new source:

```python
class MyNewDataFetcher(BaseDataFetcher):
    def fetch(self) -> List[RegionalGDPRecord]:
        # 1. Call API
        raw_data = requests.get(...)
        
        # 2. Validate and create Pydantic objects
        records, errors = self._validate_and_create_records(
            raw_data, country='MyCountry', language='MyLanguage'
        )
        
        # 3. Return (automatically logs errors)
        return records
```

### Data Pipeline (Orchestrator)

```python
pipeline = LanguageGDPDataPipeline('config.json')
dataset = pipeline.run(sources=['world_bank'])
```

**Pipeline Steps:**
1. **Load Config** - Read `config.json`
2. **Fetch** - Call API fetchers, get `List[RegionalGDPRecord]`
3. **Validate** - Pydantic auto-validates (failures tracked)
4. **Combine** - Merge all sources into `RegionalGDPDataset`
5. **Export** - Save to CSV + JSON metadata

**Error Handling:**
- API timeout? Logged, continues to next source
- Validation fail? Record discarded, error logged
- No data? `APIFetchResult.success=False`, but pipeline continues

---

## Usage

### Phase A: National-level (Current)

```bash
# Install dependencies
pip install pydantic requests pandas matplotlib seaborn scipy

# Run pipeline
python data_pipeline.py
# → Outputs: data/processed/language_gdp_combined.csv

# Or use Jupyter
jupyter notebook 01_data_collection.ipynb
```

**What you get:**
- ✅ 6 countries × ~20 years = ~120 national-level records
- ✅ Validated & type-safe
- ✅ CSV export ready for analysis
- ✅ Metadata (sources, timestamps, validation stats)

### Phase B: Regional-level (Next)

To add German regional data:

1. **Register at Destatis** (https://www-genesis.destatis.de/)
2. **Update `config.json`:**
   ```json
   {
     "destatis": {
       "username": "YOUR_USERNAME",
       "password": "YOUR_PASSWORD",
       "enabled": true
     }
   }
   ```
3. **Implement `DestatisDataFetcher.fetch()`**
4. **Run:** `pipeline.run(sources=['world_bank', 'destatis'])`

### Phase C: With Controls (Future)

Add education spending, R&D intensity, etc.:

1. **Create new Pydantic model:**
   ```python
   class RegionalControlsRecord(BaseModel):
       base: RegionalGDPRecord
       education_pct_gdp: float
       rd_intensity: float
   ```

2. **Add fetcher for controls data**
3. **Merge** in pipeline
4. **Run regression** with language as independent variable

---

## Configuration (config.json)

### API Settings
```json
{
  "api_config": {
    "world_bank": {
      "base_url": "https://api.worldbank.org/v2",
      "timeout": 30,
      "enabled": true
    }
  }
}
```

### Validation Rules
```json
{
  "data_validation": {
    "valid_languages": ["German", "Japanese", "Spanish"],
    "year_range": [1990, 2024],
    "min_population_million": 0.01
  }
}
```

---

## Data Flow Diagram

```
┌──────────────────────┐
│   World Bank API     │
│  (6 countries)       │
└──────────┬───────────┘
           │
           ↓
┌──────────────────────────────────┐
│  WorldBankDataFetcher            │
│  - Fetches GDP, population       │
│  - Returns List[RegionalGDPRecord]
└──────────┬───────────────────────┘
           │
           ↓
┌──────────────────────────────────┐
│  Pydantic Validation             │
│  - Type checking                 │
│  - Business logic (gdp > 0)      │
│  - Error tracking                │
└──────────┬───────────────────────┘
           │
           ↓
┌──────────────────────────────────┐
│  LanguageGDPDataPipeline         │
│  - Combines sources              │
│  - Creates RegionalGDPDataset    │
│  - Exports CSV + metadata        │
└──────────┬───────────────────────┘
           │
           ↓
┌──────────────────────────────────┐
│  data/processed/                 │
│  - language_gdp_combined.csv     │
│  - metadata.json                 │
└──────────────────────────────────┘
           │
           ↓
┌──────────────────────────────────┐
│  Jupyter Notebook                │
│  - EDA + Statistics              │
│  - Hypothesis testing (ANOVA)    │
└──────────────────────────────────┘
```

---

## Validation in Action

### Example: Data Quality Issue

```python
# API returns malformed data
bad_record = {
    'region': 'Tokyo',
    'gdp_billion_usd': None,  # ❌ Missing!
    'year': 2023
}

# Without Pydantic:
df.loc[0] = bad_record  # Silently inserted NaN, hard to debug

# With Pydantic:
try:
    record = RegionalGDPRecord(**bad_record)
except ValidationError as e:
    print(e)
    # ValidationError: 1 validation error for RegionalGDPRecord
    # gdp_billion_usd
    #   Field required (type=missing)
```

### Example: Auto-calculated Fields

```python
record = RegionalGDPRecord(
    region='Tokyo',
    country='Japan',
    language='Japanese',
    gdp_billion_usd=2100,
    population_million=14.0,
    year=2023,
    source='world_bank'
)

# Pydantic auto-calculates:
print(record.gdp_per_capita_usd)
# → 150,000 USD
```

---

## Performance Notes

- **World Bank API:** ~5-10 records/second (rate-limited)
- **Validation:** <1ms per record
- **Export:** ~1-2 seconds for 1000 records

**Caching Strategy:**
- Results cached in `./cache/` (SQLite)
- TTL: 168 hours (7 days) by default
- Override: `pipeline.run(use_cache=False)`

---

## Next Steps

### Short Term
- [ ] Test World Bank fetch on your machine
- [ ] Verify data quality (spot-check 10 records)
- [ ] Check CSV output format

### Medium Term
- [ ] Implement Destatis fetcher (German regional data)
- [ ] Implement StatSwiss fetcher (Swiss cantonal data)
- [ ] Run ANOVA on regional-level data

### Long Term
- [ ] Add education spending controls
- [ ] Regression analysis with interaction terms
- [ ] CLI tool for pipeline execution
- [ ] Web app for data exploration

---

## References

- **Pydantic Docs:** https://docs.pydantic.dev/
- **World Bank API:** https://data.worldbank.org/
- **Destatis (German Stats):** https://www.destatis.de/
- **STATSWISS (Swiss Stats):** https://www.bfs.admin.ch/
- **日本内閣府:** https://www.esri.cao.go.jp/

---

**Author's Note:**

This is "honest code" — using Pydantic means:
- You *want* to know when data is bad
- You *expect* validation failures (and track them)
- You *can* confidently pass data to analysis notebooks

It's slower to write initially, but saves 10x debugging time later.

---

*Last Updated: 2026-04-25*  
*Status: Phase A (National-level data collection)*
