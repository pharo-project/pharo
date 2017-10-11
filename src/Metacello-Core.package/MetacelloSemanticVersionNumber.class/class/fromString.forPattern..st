instance creation
fromString: aString forPattern: forPattern
  | new tokens preRelease build versionString identifierCount normalEnd preReleaseEnd normalComponents preReleaseComponents buildComponents |
  normalComponents := OrderedCollection new.
  preReleaseComponents := OrderedCollection new.
  buildComponents := OrderedCollection new.
  preRelease := aString indexOf: $- startingAt: 1.
  build := aString indexOf: $+ startingAt: 1.
  (build > 0 and: [ preRelease > build ])
    ifTrue: [ preRelease := 0 ].
  normalEnd := preRelease = 0
    ifTrue: [ 
      build = 0
        ifTrue: [ aString size ]
        ifFalse: [ build - 1 ] ]
    ifFalse: [ preRelease - 1 ].
  versionString := aString copyFrom: 1 to: normalEnd.
  identifierCount := 0.
  (versionString findTokens: '.')
    do: [ :subString | 
      | integer |
      forPattern
        ifTrue: [ integer := subString ]
        ifFalse: [ 
          integer := self integerFromString: subString.
          integer < 0
            ifTrue: [ 
              self
                error:
                  'invalid version number: normal version component must be integer '
                    , subString printString ] ].
      normalComponents add: integer.
      identifierCount := identifierCount + 1 ].
  (forPattern not and: [ identifierCount ~= 3 ])
    ifTrue: [ self error: 'invalid version number: normal version must have only 3 components' ].
  preReleaseEnd := build = 0
    ifTrue: [ aString size ]
    ifFalse: [ build - 1 ].
  preRelease > 0
    ifTrue: [ 
      versionString := aString copyFrom: preRelease + 1 to: preReleaseEnd.
      (versionString findTokens: '.')
        do: [ :subString | 
          (forPattern or: [ self isSemanticIdentifier: subString ])
            ifFalse: [ 
              self
                error:
                  'invalid version number: preRelease version component must be one of [0-9A-Za-z-]' ].
          preReleaseComponents
            add:
              (self extractNumericComponent: subString forPattern: forPattern) ] ].
  build > 0
    ifTrue: [ 
      versionString := aString copyFrom: build + 1 to: aString size.
      (versionString findTokens: '.')
        do: [ :subString | 
          (forPattern or: [ self isSemanticIdentifier: subString ])
            ifFalse: [ 
              self
                error:
                  'invalid version number: build version component must be one of [0-9A-Za-z-]' ].
          buildComponents
            add:
              (self extractNumericComponent: subString forPattern: forPattern) ] ].
  ^ self new
    normalVersion: normalComponents;
    preReleaseVersion: preReleaseComponents;
    buildVersion: buildComponents;
    yourself