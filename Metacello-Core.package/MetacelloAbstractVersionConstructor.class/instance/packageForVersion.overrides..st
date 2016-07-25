api spec callbacks
packageForVersion: aString overrides: aBlock
    | spec |
    spec := self project packageSpec
        name: aString;
        yourself.
    self root packages add: spec.
    self with: spec during: aBlock