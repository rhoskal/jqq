# JQQ (Pretty JSON)

A `jq`-lite clone.

```sh
λ jqq --help
jqq - a simple 'jq' clone

Usage: jqq [--indent INT] ((-f|--file FILE_PATH) | --stdin) [-d|--debug]

  Parse and format JSON input

Available options:
  --indent INT             Desired indentation level (default: 2)
  -f,--file FILE_PATH      Path to JSON file
  --stdin                  Read JSON from stdin
  -d,--debug               Show internal `JsonValue`
  -h,--help                Show this help text
```

## Examples

`jqq` commands:

```sh
jqq -f data/sample4.json

cat data/sample4.json | jqq --stdin
```

`jq` equivalent commands:

```sh
jq '.' data/sample4.json

cat data/sample4.json | jq '.'
```

## References

- https://www.json.org/json-en.html
- https://www.rfc-editor.org/rfc/rfc8259
- https://jqlang.github.io/jq/tutorial/
