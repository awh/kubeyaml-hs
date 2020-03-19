# kubeyaml-hs

A partial Haskell implementation of https://github.com/squaremo/kubeyaml, using
the [reference YAML 1.2 parser](https://github.com/orenbenkiki/yamlreference).

At this stage only the `image` subcommand is implemented as a proof of concept.

## Theory of Operation

The reference YAML parser is a direct implementation of the BNF productions in
the spec. When it has successfully parsed a document, we have two things: one,
a perfect guarantee (it is the reference parser after all) that the document is
valid YAML in all its esoteric glory, and two, a token stream enriched with
semantic information about the recursive structure and types in the document.

By filtering this token stream to include just semantic information regarding
node/scalar/mapping/sequence/pair beginnings/endings and passing it through a
second simple parser we can recover the nested data structures for programmatic
analysis and manipulation _whilst retaining the context of the wider token
stream_. This enables us to locate the tokens for specific items of interest
(for example image references) and then to concatenate the full token stream
with selective replacement of those tokens, guaranteeing the preservation of
all whitespace, comments, indicators and other YAML paraphenalia.

Since YAML is a superset of JSON, the same implementation works for JSON files
without any further special consideration.

## Prerequisites

Install `stack` from https://haskellstack.org

## Execution

### Replace image in YAML
```
$ stack run -- image --namespace default \
                     --kind Deployment \
                     --name nginx-deployment \
                     --container nginx \
                     --image foo < test.yaml
# This is some leading commentary
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: nginx-deployment
  labels:
    app: nginx
spec:
  replicas: 3
  selector:
    matchLabels:
      app: nginx
  template:
    metadata:
      labels:
        app: nginx
    spec:
      containers:
      - name: nginx

        # Comment
        image: "foo" # Trailing comment
        ports:
        - containerPort: 80
```

### Replace image in JSON
```
$ stack run -- image --namespace default \
                     --kind Deployment \
                     --name nginx-deployment \
                     --container nginx \
                     --image foo < test.json
{
    "kind": "Deployment",
    "metadata" : {
        "name": "nginx-deployment"
    },
    "spec": {
        "template": {
            "spec": {
                "containers": [
                    {
                        "name": "nginx",
                        "image": "foo"
                    }
                ]
            }
        }
    }
}
```
