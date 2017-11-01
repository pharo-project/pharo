"
MetacelloSemanticVersionNumber conforms to version 2.0.0-rc.1 of  [Semantic Versioning 2.0.0-rc.1](http://semver.org/)

The most important thing that you need to know is that: 
**A normal version number MUST take the form X.Y.Z where X, Y, and Z are non-negative integers.**

**Semantic Versioning Specification** extracted from [Semantic versioning 2.0.0-rc.1](https://github.com/mojombo/semver/blob/3c7f2e8df747ea0ca15208fdfc90e3275240184f/semver.md):

Semantic Versioning Specification (SemVer)
------------------------------------------

The key words ""MUST"", ""MUST NOT"", ""REQUIRED"", ""SHALL"", ""SHALL NOT"", ""SHOULD"",
""SHOULD NOT"", ""RECOMMENDED"", ""MAY"", and ""OPTIONAL"" in this document are to be
interpreted as described in RFC 2119.

1. Software using Semantic Versioning MUST declare a public API. This API
could be declared in the code itself or exist strictly in documentation.
However it is done, it should be precise and comprehensive.

1. A normal version number MUST take the form X.Y.Z where X, Y, and Z are
non-negative integers. X is the major version, Y is the minor version, and Z
is the patch version. Each element MUST increase numerically by increments of
one. For instance: 1.9.0 -> 1.10.0 -> 1.11.0.

1. Once a versioned package has been released, the contents of that version
MUST NOT be modified. Any modifications must be released as a new version.

1. Major version zero (0.y.z) is for initial development. Anything may change
at any time. The public API should not be considered stable.

1. Version 1.0.0 defines the public API. The way in which the version number
is incremented after this release is dependent on this public API and how it
changes.

1. Patch version Z (x.y.Z | x > 0) MUST be incremented if only backwards
compatible bug fixes are introduced. A bug fix is defined as an internal
change that fixes incorrect behavior.

1. Minor version Y (x.Y.z | x > 0) MUST be incremented if new, backwards
compatible functionality is introduced to the public API. It MUST be
incremented if any public API functionality is marked as deprecated. It MAY be
incremented if substantial new functionality or improvements are introduced
within the private code. It MAY include patch level changes. Patch version
MUST be reset to 0 when minor version is incremented.

1. Major version X (X.y.z | X > 0) MUST be incremented if any backwards
incompatible changes are introduced to the public API. It MAY include minor
and patch level changes. Patch and minor version MUST be reset to 0 when major
version is incremented.

1. A pre-release version MAY be denoted by appending a hyphen and a series of
dot separated identifiers immediately following the patch version. Identifiers
MUST be comprised of only ASCII alphanumerics and dash [0-9A-Za-z-].
Pre-release versions satisfy but have a lower precedence than the associated
normal version. Examples: 1.0.0-alpha, 1.0.0-alpha.1, 1.0.0-0.3.7,
1.0.0-x.7.z.92.

1. A build version MAY be denoted by appending a plus sign and a series of dot
separated identifiers immediately following the patch version or pre-release
version. Identifiers MUST be comprised of only ASCII alphanumerics and dash
[0-9A-Za-z-]. Build versions satisfy and have a higher precedence than the
associated normal version. Examples: 1.0.0+build.1, 1.3.7+build.11.e0f985a.

1. Precedence MUST be calculated by separating the version into major, minor,
patch, pre-release, and build identifiers in that order. Major, minor, and
patch versions are always compared numerically. Pre-release and build version
precedence MUST be determined by comparing each dot separated identifier as
follows: identifiers consisting of only digits are compared numerically and
identifiers with letters or dashes are compared lexically in ASCII sort order.
Numeric identifiers always have lower precedence than non-numeric identifiers.
Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-beta.2 < 1.0.0-beta.11 <
1.0.0-rc.1 < 1.0.0-rc.1+build.1 < 1.0.0 < 1.0.0+0.3.7 < 1.3.7+build <
1.3.7+build.2.b8f12d7 < 1.3.7+build.11.e0f985a.

About
-----

The Semantic Versioning specification is authored by [Tom
Preston-Werner](http://tom.preston-werner.com), inventor of Gravatars and
cofounder of GitHub.

If you'd like to leave feedback, please [open an issue on
GitHub](https://github.com/mojombo/semver/issues).


License
-------

Creative Commons - CC BY 3.0
http://creativecommons.org/licenses/by/3.0/

"
Class {
	#name : #MetacelloSemanticVersionNumber,
	#superclass : #Magnitude,
	#instVars : [
		'normalVersion',
		'preReleaseVersion',
		'buildVersion'
	],
	#category : 'Metacello-Core-Model'
}

{ #category : #private }
MetacelloSemanticVersionNumber class >> extractNumericComponent: subString forPattern: forPattern [
    "$. separated components are integers"

    | number |
    forPattern
        ifTrue: [ ^ subString ].
    number := [ subString asNumber ]
        on: Error
        do: [ :ex | ex return: subString ].
    ^ number asString = subString
        ifTrue: [ number ]
        ifFalse: [ subString ]
]

{ #category : #'instance creation' }
MetacelloSemanticVersionNumber class >> fromString: aString [
  | svn |
  svn := self fromString: aString forPattern: false.
  self validateVersionNumber: svn against: aString.
  ^ svn
]

{ #category : #'instance creation' }
MetacelloSemanticVersionNumber class >> fromString: aString forPattern: forPattern [
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
]

{ #category : #private }
MetacelloSemanticVersionNumber class >> integerFromString: aString [
  aString
    detect: [ :char | char isDigit not ]
    ifNone: [ 
      | integer |
      integer := aString asInteger.
      ((aString at: 1) = $0 and: [ aString size > 1 ])
        ifTrue: [ 
          self
            error:
              'invalid version number: normal version component must not have leading 0s'
                , aString asString ].
      ^ integer ].
  self
    error:
      'invalid version number: normal version component must be integer '
        , aString asString
]

{ #category : #private }
MetacelloSemanticVersionNumber class >> isSemanticIdentifier: aString [
    "whether the receiver is composed entirely of alphanumerics"

    aString
        do: [ :c | 
            c isAlphaNumeric
                ifFalse: [ 
                    c = $-
                        ifFalse: [ ^ false ] ] ].
    ^ true
]

{ #category : #private }
MetacelloSemanticVersionNumber class >> validateVersionNumber: svn against: aString [
  svn printString = aString
    ifFalse: [ 
      self
        error:
          'The printString of a semantic version number should be equal to the source version string' ]
]

{ #category : #comparing }
MetacelloSemanticVersionNumber >> < aMetacelloVersionNumber [
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    ^ self compareLessThan: aMetacelloVersionNumber
]

{ #category : #comparing }
MetacelloSemanticVersionNumber >> = aMetacelloVersionNumber [
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    ^ self compareEqualTo: aMetacelloVersionNumber
]

{ #category : #accessing }
MetacelloSemanticVersionNumber >> approximateBase [

	| base |
	base := self copyFrom: 1 to: self size - 1.
	base at: base size put: (base at: base size) + 1.
	^base
]

{ #category : #converting }
MetacelloSemanticVersionNumber >> asMetacelloSemanticVersionNumber [
    ^ self
]

{ #category : #printing }
MetacelloSemanticVersionNumber >> asString [
	"Answer a string that represents the receiver."

	^ self printString
]

{ #category : #accessing }
MetacelloSemanticVersionNumber >> buildVersion [
    buildVersion ifNil: [ buildVersion := #() ].
    ^ buildVersion
]

{ #category : #accessing }
MetacelloSemanticVersionNumber >> buildVersion: anObject [
	buildVersion := anObject
]

{ #category : #private }
MetacelloSemanticVersionNumber >> compareEqualTo: aMetacelloVersionNumber [
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    (self compareEqualTo: self normalVersion other: aMetacelloVersionNumber normalVersion)
        ifFalse: [ ^ false ].
    (self compareEqualTo: self preReleaseVersion other: aMetacelloVersionNumber preReleaseVersion)
        ifFalse: [ ^ false ].
    ^ self compareEqualTo: self buildVersion other: aMetacelloVersionNumber buildVersion
]

{ #category : #private }
MetacelloSemanticVersionNumber >> compareEqualTo: myComponents other: otherComponents [
    | mySize |
    mySize := myComponents size.
    mySize = otherComponents size
        ifFalse: [ ^ false ].
    1 to: mySize do: [ :i | 
        (myComponents at: i) = (otherComponents at: i)
            ifFalse: [ ^ false ] ].
    ^ true
]

{ #category : #private }
MetacelloSemanticVersionNumber >> compareLessThan: aMetacelloVersionNumber [
    | myComponents otherComponents defaultResult |
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    myComponents := self normalVersion.
    otherComponents := aMetacelloVersionNumber normalVersion.
    defaultResult := true.
    (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ defaultResult := false ]
        ifFalse: [ 
            (self compareLessThan: myComponents other: otherComponents version: #'normal')
                ifFalse: [ ^ false ] ].
    myComponents := self preReleaseVersion.
    otherComponents := aMetacelloVersionNumber preReleaseVersion.
    (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ 
            myComponents size > 0
                ifTrue: [ defaultResult := false ] ]
        ifFalse: [ ^ self compareLessThan: myComponents other: otherComponents version: #'preRelease' ].
    myComponents := self buildVersion.
    otherComponents := aMetacelloVersionNumber buildVersion.
    ^ (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ defaultResult ]
        ifFalse: [ self compareLessThan: myComponents other: otherComponents version: #'build' ]
]

{ #category : #private }
MetacelloSemanticVersionNumber >> compareLessThan: myComponents other: otherComponents version: version [
    | mySize aSize commonSize count more |
    mySize := myComponents size.
    aSize := otherComponents size.
    commonSize := mySize min: aSize.
    count := 0.
    more := true.
    [ more and: [ count < commonSize ] ]
        whileTrue: [ 
            (myComponents at: count + 1) = (otherComponents at: count + 1)
                ifTrue: [ count := count + 1 ]
                ifFalse: [ more := false ] ].
    count < commonSize
        ifTrue: [ ^ (myComponents at: count + 1) metacelloSemanticVersionComponentLessThan: (otherComponents at: count + 1) ].
    mySize < aSize
        ifTrue: [ 
            mySize = 0
                ifTrue: [ 
                    #'preRelease' == version
                        ifTrue: [ ^ false ].
                    ^ true ].
            (myComponents at: commonSize) = (otherComponents at: commonSize)
                ifFalse: [ ^ true ].
            ^ true ]
        ifFalse: [ 
            mySize = aSize
                ifTrue: [ ^ false ].
            aSize = 0
                ifTrue: [ 
                    #'build' == version
                        ifTrue: [ ^ false ].
                    ^ true ].
            (myComponents at: commonSize) = (otherComponents at: commonSize)
                ifFalse: [ ^ false ].
            ^ true ]
]

{ #category : #copying }
MetacelloSemanticVersionNumber >> copyFrom: start to: stop [ 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize new j |
	newSize := stop - start + 1.
	new := self species new: newSize.
	j := 0.
	start to: stop do: [:i |
		new at: j + 1 put: (self at: i).
		j := j + 1 ].
	^new
]

{ #category : #operations }
MetacelloSemanticVersionNumber >> decrementMajorVersion [
  self decrementNormalVersionAt: 1
]

{ #category : #operations }
MetacelloSemanticVersionNumber >> decrementMinorVersion [
  self decrementNormalVersionAt: 2
]

{ #category : #operations }
MetacelloSemanticVersionNumber >> decrementMinorVersionNumber [
  self decrementNormalVersionAt: 3
]

{ #category : #private }
MetacelloSemanticVersionNumber >> decrementNormalVersionAt: index [
  | int col |
  col := self normalVersion.
  int := col at: index.
  int > 0
    ifTrue: [ col at: index put: int - 1 ]
]

{ #category : #operations }
MetacelloSemanticVersionNumber >> decrementPatchVersion [
  self decrementNormalVersionAt: 3
]

{ #category : #enumerating }
MetacelloSemanticVersionNumber >> do: aBlock [ 
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | aBlock value: (self at: index)]
]

{ #category : #enumerating }
MetacelloSemanticVersionNumber >> do: elementBlock separatedBy: separatorBlock [
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	| beforeFirst | 
	beforeFirst := true.
	self do:
		[:each |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: each]
]

{ #category : #comparing }
MetacelloSemanticVersionNumber >> hash [
    ^ self versionComponents hash
]

{ #category : #operations }
MetacelloSemanticVersionNumber >> incrementMajorVersion [
  self incrementNormalVersionAt: 1
]

{ #category : #operations }
MetacelloSemanticVersionNumber >> incrementMinorVersion [
  self incrementNormalVersionAt: 2
]

{ #category : #operations }
MetacelloSemanticVersionNumber >> incrementMinorVersionNumber [
  self incrementNormalVersionAt: 3
]

{ #category : #private }
MetacelloSemanticVersionNumber >> incrementNormalVersionAt: index [
  | int col |
  col := self normalVersion.
  int := col at: index.
  col at: index put: int + 1
]

{ #category : #operations }
MetacelloSemanticVersionNumber >> incrementPatchVersion [
  self incrementNormalVersionAt: 3
]

{ #category : #comparing }
MetacelloSemanticVersionNumber >> match: aVersionPattern [
    "Answer whether the version number of the receiver matches the given pattern string.

	 A Metacello version number is made up of version sequences delimited by the characters $. and $-.
	 The $. introduces a numeric version sequence and $- introduces an alphanumeric version sequence.
	 
	 A version pattern is made up of version pattern match sequences. also delimited by the characters $. 
	 and $-.. Each pattern match sequence is tested against the corresponding version sequence of the 
	 receiver, using the 'standard' pattern matching rules. All sequences must answer true for a match.
	
	 The special pattern sequence '?' is a match for the corresponding version sequence and all subsequent 
	 version sequences. '?' as the version pattern matches all versions. No more version pattern 
	 sequences are permitted once the '?' sequence is used. If used, it is the last version pattern
	 sequence. "

    | patternVersion mySize patternSize components |
    patternVersion := (self class fromString: aVersionPattern forPattern: true) versionComponents.
    components := self versionComponents.
    mySize := components size.
    patternSize := patternVersion size.
    mySize = patternSize
        ifFalse: [ 
            mySize < patternSize
                ifTrue: [ ^ false ].
            (patternVersion at: patternSize) ~= '?'
                ifTrue: [ ^ false ].
            mySize := patternSize ].
    1 to: mySize do: [ :i | 
        | pattern |
        pattern := (patternVersion at: i) asString.
        pattern = '?'
            ifTrue: [ 
                i = mySize
                    ifFalse: [ ^ self error: 'Invalid version match pattern: ' , aVersionPattern printString ] ]
            ifFalse: [ 
                (pattern match: (components at: i) asString)
                    ifFalse: [ ^ false ] ] ].
    ^ true
]

{ #category : #accessing }
MetacelloSemanticVersionNumber >> normalVersion [
    normalVersion ifNil: [ normalVersion := #() ].
    ^ normalVersion
]

{ #category : #accessing }
MetacelloSemanticVersionNumber >> normalVersion: anObject [
	normalVersion := anObject
]

{ #category : #copying }
MetacelloSemanticVersionNumber >> postCopy [
  normalVersion := normalVersion copy.
  preReleaseVersion := preReleaseVersion copy.
  buildVersion := buildVersion copy
]

{ #category : #accessing }
MetacelloSemanticVersionNumber >> preReleaseVersion [
    preReleaseVersion ifNil: [ preReleaseVersion := #() ].
    ^ preReleaseVersion
]

{ #category : #accessing }
MetacelloSemanticVersionNumber >> preReleaseVersion: anObject [
	preReleaseVersion := anObject
]

{ #category : #printing }
MetacelloSemanticVersionNumber >> print: components prefix: prefixChar on: aStream [
    | beforeFirst |
    beforeFirst := true.
    components
        do: [ :component | 
            beforeFirst
                ifTrue: [ 
                    beforeFirst := false.
                    prefixChar ifNotNil: [ aStream nextPut: prefixChar ] ]
                ifFalse: [ aStream nextPut: $. ].
            aStream nextPutAll: component asString ]
]

{ #category : #printing }
MetacelloSemanticVersionNumber >> printOn: aStream [
    self print: self normalVersion prefix: nil on: aStream.
    self print: self preReleaseVersion prefix: $- on: aStream.
    self print: self buildVersion prefix: $+ on: aStream
]

{ #category : #private }
MetacelloSemanticVersionNumber >> versionComponents [
    ^ self normalVersion , self preReleaseVersion , self buildVersion
]

{ #category : #accessing }
MetacelloSemanticVersionNumber >> versionString [

	| strm |
	strm := WriteStream on: String new.
	self printOn: strm.
	^strm contents
]

{ #category : #comparing }
MetacelloSemanticVersionNumber >> ~> aMetacelloVersionNumber [

	aMetacelloVersionNumber size == 1 ifTrue: [ ^false ].
	^self >= aMetacelloVersionNumber and: [ self < aMetacelloVersionNumber approximateBase ]
]
