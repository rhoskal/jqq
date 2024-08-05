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

equivalent `jq` commands:

```sh
jq '.' data/sample4.json

cat data/sample4.json | jq '.'
```

## Debugging

```sh
jqq -f data/sample8.json -d
{
  "😀": {
    "name": "grinning face",
    "slug": "grinning_face",
    "group": "Smileys & Emotion",
    "emoji_version": "2.0",
    "unicode_version": "6.1",
    "skin_tone_support": false
  },
  ...
}
JObject
  [
    ( "ð\x9f\x98\x80", JObject
      [
        ( "name", JString "grinning face" ),
        ( "slug", JString "grinning_face" ),
        ( "group", JString "Smileys & Emotion" ),
        ( "emoji_version", JString "2.0" ),
        ( "unicode_version", JString "6.1" ),
        ( "skin_tone_support", JBool False )
      ]
    ),
    ...
  ]
```

## References

- https://www.json.org/json-en.html
- https://www.rfc-editor.org/rfc/rfc8259
- https://jqlang.github.io/jq/tutorial/
