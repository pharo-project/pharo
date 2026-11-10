"
I am a semantic version: a normal version (major, minor, patch), prerelease identifiers and build metadata, ordered by semver precedence (numeric identifiers compare as numbers, a prerelease comes before its release).

Read from a git tag (fromTag:), I keep the tag. The canonical form is vMAJOR.MINOR.PATCH with an optional -prerelease and no leading zeros; other forms are accepted, to deprecate: two numbers (v1.2 is v1.2.0), leading zeros (v1.02 is v1.2), no v (1.2.7 is v1.2.7). isCanonical tells them apart. A floating tag (vX.Y.x, vX.x.x) reads as the first version of its series (fromSeriesTag:).

fromString: reads a strict semver string, without v.
"
Class {
	#name : 'MetacelloSemanticVersionNumber',
	#superclass : 'Magnitude',
	#instVars : [
		'normalVersion',
		'preReleaseVersion',
		'buildVersion',
		'string'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'versions' }
MetacelloSemanticVersionNumber class >> canonicalPattern [
	"A canonical version: vX.Y.Z with an optional prerelease, without leading zeros"

	^ '^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(-[0-9A-Za-z.-]*[0-9A-Za-z])?$' asRegex
]

{ #category : 'private' }
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

{ #category : 'versions' }
MetacelloSemanticVersionNumber class >> fromSeries: aString [
	"A floating version counts as the first version of its series: vX.Y.x as vX.Y.0, vX.x.x as vX.0.0"

	| pattern minor |
	pattern := self seriesPattern.
	(pattern matches: aString) ifFalse: [ ^ self error: aString printString , ' is not a vMAJOR.MINOR.x or vMAJOR.x.x string' ].
	minor := pattern subexpression: 3.
	^ self
		  major: (pattern subexpression: 2) asNumber
		  minor: (minor = 'x'
				   ifTrue: [ 0 ]
				   ifFalse: [ minor asNumber ])
		  patch: 0
		  prerelease: nil
]

{ #category : 'instance creation' }
MetacelloSemanticVersionNumber class >> fromString: aString [
  | svn |
  svn := self fromString: aString forPattern: false.
  self validateVersionNumber: svn against: aString.
  ^ svn
]

{ #category : 'instance creation' }
MetacelloSemanticVersionNumber class >> fromString: aString forPattern: forPattern [
  | preRelease build versionString identifierCount normalEnd preReleaseEnd normalComponents preReleaseComponents buildComponents |
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

{ #category : 'instance creation' }
MetacelloSemanticVersionNumber class >> fromVersionString: aString [
	"The version aString names: vX.Y.Z with an optional prerelease, or a tolerated form (see versionPattern). I keep aString"

	| pattern |
	pattern := self versionPattern.
	(pattern matches: aString) ifFalse: [ ^ self error: aString printString , ' is not a vMAJOR.MINOR.PATCH or vMAJOR.MINOR version' ].
	^ (self
		   major: (pattern subexpression: 2) asNumber
		   minor: (pattern subexpression: 3) asNumber
		   patch: ((pattern subexpression: 5) ifNil: [ 0 ] ifNotNil: [ :digits | digits asNumber ])
		   prerelease: ((pattern subexpression: 6) ifNotNil: [ :suffix | suffix allButFirst ]))
		  setString: aString;
		  yourself
]

{ #category : 'private' }
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

{ #category : 'versions' }
MetacelloSemanticVersionNumber class >> isCanonical: aString [

	^ self canonicalPattern matches: aString
]

{ #category : 'private' }
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

{ #category : 'versions' }
MetacelloSemanticVersionNumber class >> isSemanticVersion: aString [

	^ self versionPattern matches: aString
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber class >> isSeries: aString [

	^ self seriesPattern matches: aString
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber class >> major: major minor: minor patch: patch prerelease: aStringOrNil [

	^ self new
		  normalVersion: {
				  major.
				  minor.
				  patch } asOrderedCollection;
		  preReleaseVersion: (aStringOrNil
				   ifNil: [ OrderedCollection new ]
				   ifNotNil: [
					   ((aStringOrNil findTokens: '.') collect: [ :each | self extractNumericComponent: each forPattern: false ])
						   asOrderedCollection ]);
		  buildVersion: OrderedCollection new;
		  yourself
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber class >> seriesPattern [
	"A floating version: vX.Y.x, or vX.x.x for a whole major"

	^ '^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*|x)\.x$' asRegex
]

{ #category : 'private' }
MetacelloSemanticVersionNumber class >> validateVersionNumber: svn against: aString [

	svn versionString = aString ifFalse: [
		self error: 'The version string of a semantic version number should be equal to the source version string' ]
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber class >> versionPattern [
	"What a string may be: vX.Y.Z with an optional prerelease, vX.Y, with or without the v, leading zeros allowed"

	^ '^v?([0-9]+)\.([0-9]+)(\.([0-9]+)(-[0-9A-Za-z.-]*[0-9A-Za-z])?)?$' asRegex
]

{ #category : 'comparing' }
MetacelloSemanticVersionNumber >> < aMetacelloVersionNumber [
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    ^ self compareLessThan: aMetacelloVersionNumber
]

{ #category : 'comparing' }
MetacelloSemanticVersionNumber >> = aMetacelloVersionNumber [
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    ^ self compareEqualTo: aMetacelloVersionNumber
]

{ #category : 'accessing' }
MetacelloSemanticVersionNumber >> approximateBase [

	| base |
	base := self copyFrom: 1 to: self size - 1.
	base at: base size put: (base at: base size) + 1.
	^base
]

{ #category : 'accessing' }
MetacelloSemanticVersionNumber >> buildVersion [
    buildVersion ifNil: [ buildVersion := #() ].
    ^ buildVersion
]

{ #category : 'accessing' }
MetacelloSemanticVersionNumber >> buildVersion: anObject [
	buildVersion := anObject
]

{ #category : 'private' }
MetacelloSemanticVersionNumber >> compareEqualTo: aMetacelloVersionNumber [
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    (self compareEqualTo: self normalVersion other: aMetacelloVersionNumber normalVersion)
        ifFalse: [ ^ false ].
    (self compareEqualTo: self preReleaseVersion other: aMetacelloVersionNumber preReleaseVersion)
        ifFalse: [ ^ false ].
    ^ self compareEqualTo: self buildVersion other: aMetacelloVersionNumber buildVersion
]

{ #category : 'private' }
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

{ #category : 'private' }
MetacelloSemanticVersionNumber >> compareLessThan: aMetacelloVersionNumber [
	"Semver precedence: the normal versions decide when they differ; then a prerelease comes before its release, and prereleases compare identifier by identifier; then build metadata"

	| myComponents otherComponents defaultResult |
	aMetacelloVersionNumber species = self species ifFalse: [ ^ false ].
	myComponents := self normalVersion.
	otherComponents := aMetacelloVersionNumber normalVersion.
	(self compareEqualTo: myComponents other: otherComponents) ifFalse: [
		^ self compareLessThan: myComponents other: otherComponents version: #normal ].
	defaultResult := false.
	myComponents := self preReleaseVersion.
	otherComponents := aMetacelloVersionNumber preReleaseVersion.
	(self compareEqualTo: myComponents other: otherComponents) ifFalse: [
		^ self compareLessThan: myComponents other: otherComponents version: #preRelease ].
	myComponents := self buildVersion.
	otherComponents := aMetacelloVersionNumber buildVersion.
	^ (self compareEqualTo: myComponents other: otherComponents)
		  ifTrue: [ defaultResult ]
		  ifFalse: [ self compareLessThan: myComponents other: otherComponents version: #build ]
]

{ #category : 'private' }
MetacelloSemanticVersionNumber >> compareLessThan: myComponents other: otherComponents version: version [
	| mySize aSize commonSize count more |
	mySize := myComponents size.
	aSize := otherComponents size.
	commonSize := mySize min: aSize.
	count := 0.
	more := true.
	[ more and: [ count < commonSize ] ]
		whileTrue: [ (myComponents at: count + 1) = (otherComponents at: count + 1)
				ifTrue: [ count := count + 1 ]
				ifFalse: [ more := false ] ].
	count < commonSize ifTrue: [ ^ (myComponents at: count + 1) metacelloSemanticVersionComponentLessThan: (otherComponents at: count + 1) ].
	^ mySize < aSize
		ifTrue: [ mySize = 0
				ifTrue: [ #preRelease == version ifTrue: [ ^ false ].
					^ true ].
			(myComponents at: commonSize) = (otherComponents at: commonSize) ifFalse: [ ^ true ].
			true ]
		ifFalse: [ mySize = aSize ifTrue: [ ^ false ].
			aSize = 0
				ifTrue: [ #build == version ifTrue: [ ^ false ].
					^ true ].
			(myComponents at: commonSize) = (otherComponents at: commonSize) ifFalse: [ ^ false ].
			true ]
]

{ #category : 'copying' }
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

{ #category : 'private' }
MetacelloSemanticVersionNumber >> decrementNormalVersionAt: index [
  | int col |
  col := self normalVersion.
  int := col at: index.
  int > 0
    ifTrue: [ col at: index put: int - 1 ]
]

{ #category : 'enumerating' }
MetacelloSemanticVersionNumber >> do: aBlock [ 
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | aBlock value: (self at: index)]
]

{ #category : 'enumerating' }
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

{ #category : 'comparing' }
MetacelloSemanticVersionNumber >> hash [
    ^ self versionComponents hash
]

{ #category : 'private' }
MetacelloSemanticVersionNumber >> incrementNormalVersionAt: index [
  | int col |
  col := self normalVersion.
  int := col at: index.
  col at: index put: int + 1
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber >> isCanonical [
	"Whether my string is vX.Y.Z without leading zeros; other spellings are accepted, to deprecate"

	^ self class isCanonical: self string
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber >> isPrerelease [

	^ self preReleaseVersion isNotEmpty
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber >> major [

	^ self normalVersion at: 1
]

{ #category : 'comparing' }
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

{ #category : 'versions' }
MetacelloSemanticVersionNumber >> minor [

	^ self normalVersion at: 2
]

{ #category : 'accessing' }
MetacelloSemanticVersionNumber >> normalVersion [
    normalVersion ifNil: [ normalVersion := #() ].
    ^ normalVersion
]

{ #category : 'accessing' }
MetacelloSemanticVersionNumber >> normalVersion: anObject [
	normalVersion := anObject
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber >> patch [

	^ self normalVersion at: 3
]

{ #category : 'copying' }
MetacelloSemanticVersionNumber >> postCopy [
  normalVersion := normalVersion copy.
  preReleaseVersion := preReleaseVersion copy.
  buildVersion := buildVersion copy
]

{ #category : 'accessing' }
MetacelloSemanticVersionNumber >> preReleaseVersion [
    preReleaseVersion ifNil: [ preReleaseVersion := #() ].
    ^ preReleaseVersion
]

{ #category : 'accessing' }
MetacelloSemanticVersionNumber >> preReleaseVersion: anObject [
	preReleaseVersion := anObject
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber >> prerelease [
	"My prerelease identifiers as written, or nil"

	self isPrerelease ifFalse: [ ^ nil ].
	^ '.' join: (self preReleaseVersion collect: [ :each | each asString ])
]

{ #category : 'printing' }
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

{ #category : 'printing' }
MetacelloSemanticVersionNumber >> printOn: aStream [
	"As a version: vMAJOR.MINOR.PATCH, or the string I was read from (see versionString for the semver alone)"

	aStream nextPutAll: self string
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber >> setString: aString [

	string := aString
]

{ #category : 'versions' }
MetacelloSemanticVersionNumber >> string [
	"The git string I was read from, or the canonical vX.Y.Z"

	string ifNotNil: [ ^ string ].
	^ String streamContents: [ :stream |
			  stream
				  nextPut: $v;
				  print: self major;
				  nextPut: $.;
				  print: self minor;
				  nextPut: $.;
				  print: self patch.
			  self prerelease ifNotNil: [ :identifiers |
					  stream
						  nextPut: $-;
						  nextPutAll: identifiers ] ]
]

{ #category : 'private' }
MetacelloSemanticVersionNumber >> versionComponents [
    ^ self normalVersion , self preReleaseVersion , self buildVersion
]

{ #category : 'accessing' }
MetacelloSemanticVersionNumber >> versionString [
	"The semver alone: MAJOR.MINOR.PATCH, then -prerelease and +build when there are"

	^ String streamContents: [ :stream |
			  self print: self normalVersion prefix: nil on: stream.
			  self print: self preReleaseVersion prefix: $- on: stream.
			  self print: self buildVersion prefix: $+ on: stream ]
]

{ #category : 'comparing' }
MetacelloSemanticVersionNumber >> ~> aMetacelloVersionNumber [

	aMetacelloVersionNumber size == 1 ifTrue: [ ^false ].
	^self >= aMetacelloVersionNumber and: [ self < aMetacelloVersionNumber approximateBase ]
]
