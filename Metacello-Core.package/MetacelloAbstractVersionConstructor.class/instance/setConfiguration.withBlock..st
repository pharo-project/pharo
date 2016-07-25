api callbacks
setConfiguration: aString withBlock: aBlock
    | spec projectSpec |
    projectSpec := self project configurationOfProjectSpec
        name: aString;
        yourself.
    spec := self project projectReferenceSpec
        name: aString;
        projectReference: projectSpec;
        yourself.
    self root packages merge: spec.
    self with: projectSpec during: aBlock