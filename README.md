# kubeyaml-hs

A Haskell implementation of https://github.com/squaremo/kubeyaml, using the [reference YAML 1.2 parser](https://github.com/orenbenkiki/yamlreference).

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
