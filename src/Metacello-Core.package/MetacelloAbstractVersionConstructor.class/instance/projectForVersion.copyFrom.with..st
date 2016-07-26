api spec callbacks
projectForVersion: aString copyFrom: oldSpecName with: aBlock
    | spec projectSpec |
    projectSpec := self project projectSpec
        name: aString;
        yourself.
    spec := self project projectReferenceSpec
        name: aString;
        projectReference: projectSpec;
        yourself.
    self root packages copy: oldSpecName to: spec.
    self with: projectSpec during: aBlock