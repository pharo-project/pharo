api spec callbacks
packageForVersion: aString
    | spec |
    spec := self project packageSpec
        name: aString;
        yourself.
    self root packages add: spec