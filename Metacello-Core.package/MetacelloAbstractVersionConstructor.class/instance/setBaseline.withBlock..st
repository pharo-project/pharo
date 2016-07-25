api callbacks
setBaseline: aString withBlock: aBlock
    | projectSpec |
    projectSpec := self setBaseline: aString.
    self with: projectSpec during: aBlock