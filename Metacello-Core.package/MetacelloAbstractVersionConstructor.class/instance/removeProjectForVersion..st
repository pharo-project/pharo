api spec callbacks
removeProjectForVersion: aString
    | spec |
    spec := self project projectReferenceSpec
        name: aString;
        yourself.
    self root packages remove: spec