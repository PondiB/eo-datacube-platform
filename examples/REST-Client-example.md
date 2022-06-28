### REST calls

load_collection
```bash
curl -X POST "http://127.0.0.1:8000/v1/processes/open-eo/load_collection?id=sentinel-s2-l2a-cogs&spatial_extent=7.1%2C51.8%2C7.2%2C52.8&temporal_extent=2021-01-01%2F2021-06-30&spatial_resolution=250&temporal_resolution=P1M" -H "accept: application/json" -d ""

```