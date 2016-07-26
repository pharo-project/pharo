printing
printOn: aStream
  | label versionString descriptions |
  self
    configurationProjectSpecIfPresent: [ :spec | 
      label := spec className.
      versionString := spec versionString ]
    ifAbsent: [ 
      "baseline"
      label := self baselineProjectSpec className.
      versionString := '[baseline]' ].
  aStream nextPutAll: label.
  versionString
    ifNil: [ aStream nextPutAll: ' --no version specified--' ]
    ifNotNil: [ 
      aStream
        space;
        nextPutAll: versionString ].
  (descriptions := self repositoryDescriptions) isEmpty
    ifTrue: [ ^ self ].
  aStream nextPutAll: ' from '.
  descriptions size = 1
    ifTrue: [ aStream nextPutAll: descriptions first ]
    ifFalse: [ 
      aStream nextPut: ${.
      descriptions do: [ :description | aStream nextPutAll: description ].
      aStream nextPut: $} ]