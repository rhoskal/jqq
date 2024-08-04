# JQQ (Pretty JSON)

A `jq`-lite clone.

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
