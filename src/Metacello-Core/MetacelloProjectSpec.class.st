Class {
	#name : #MetacelloProjectSpec,
	#superclass : #MetacelloSpec,
	#instVars : [
		'name',
		'className',
		'versionString',
		'operator',
		'loads',
		'preLoadDoIt',
		'postLoadDoIt'
	],
	#category : #'Metacello-Core-Specs'
}

{ #category : #adding }
MetacelloProjectSpec >> addToMetacelloPackages: aMetacelloPackagesSpec [

	| spec |
	spec := (aMetacelloPackagesSpec project projectReferenceSpec)
			name: self name;
			projectReference: self copy;
			yourself.
	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec addMember 
			name: spec name;
			spec: spec;
			yourself)
]

{ #category : #scripting }
MetacelloProjectSpec >> canDowngradeTo: aMetacelloProjectSpec [
  (self className = aMetacelloProjectSpec className
    and: [ self operator == aMetacelloProjectSpec operator ])
    ifFalse: [ ^ false ].
  self versionOrNil == nil
    ifTrue: [ 
      "https://github.com/dalehenrich/metacello-work/issues/198#issuecomment-21737458"
      ^ true ].
  ^ (self compareVersions: aMetacelloProjectSpec usingOperator: self operator)
    not
]

{ #category : #scripting }
MetacelloProjectSpec >> canUpgradeTo: aMetacelloProjectSpec [
  (self className = aMetacelloProjectSpec className
    and: [ self operator == aMetacelloProjectSpec operator ])
    ifFalse: [ ^ false ].
  self versionOrNil == nil
    ifTrue: [ 
      "https://github.com/dalehenrich/metacello-work/issues/198#issuecomment-21737458"
      ^ true ].
  ^ self compareVersions: aMetacelloProjectSpec usingOperator: self operator
]

{ #category : #querying }
MetacelloProjectSpec >> className [
    className ifNil: [ self name ifNotNil: [ self className: self constructClassName ] ].
    ^ className
]

{ #category : #accessing }
MetacelloProjectSpec >> className: aString [
    self shouldBeMutable.
    className := aString
]

{ #category : #construction }
MetacelloProjectSpec >> className: aString constructor: aVersionConstructor [
    aVersionConstructor classNameForProject: aString
]

{ #category : #scripting }
MetacelloProjectSpec >> compareEqual: aMetacelloProjectSpec [
  "name className versionString operator loads preLoadDoIt postLoadDoIt"

  ^ self className = aMetacelloProjectSpec className
    and: [ 
      (self compareVersionsEqual: aMetacelloProjectSpec)
        and: [ 
          self operator == aMetacelloProjectSpec operator
            and: [ 
              self loads = aMetacelloProjectSpec loads
                and: [ 
                  self preLoadDoIt value == aMetacelloProjectSpec preLoadDoIt value
                    and: [ self postLoadDoIt value == aMetacelloProjectSpec postLoadDoIt value ] ] ] ] ]
]

{ #category : #scripting }
MetacelloProjectSpec >> compareVersions: aMetacelloProjectSpec usingOperator: anOperator [
  ^ aMetacelloProjectSpec versionOrNil == nil
    ifTrue: [ 
      "https://github.com/dalehenrich/metacello-work/issues/199#issuecomment-21739622"
      aMetacelloProjectSpec versionString asMetacelloVersionNumber
        perform: anOperator
        with: self version versionNumber ]
    ifFalse: [ ^ aMetacelloProjectSpec version perform: anOperator with: self version ]
]

{ #category : #scripting }
MetacelloProjectSpec >> compareVersionsEqual: aMetacelloProjectSpec [
  | vrsn otherVrsn |
  vrsn := self versionOrNil.
  otherVrsn := aMetacelloProjectSpec versionOrNil.
  vrsn ifNil: [ ^ vrsn = otherVrsn ].
  otherVrsn ifNil: [ ^ false ].
  ^ vrsn versionNumber = otherVrsn versionNumber
]

{ #category : #printing }
MetacelloProjectSpec >> configHasVersionString [
  ^ self versionString ~~ nil
]

{ #category : #printing }
MetacelloProjectSpec >> configMethodBodyOn: aStream indent: indent [
  ^ self configMethodBodyOn: aStream indent: indent fromShortCut: false
]

{ #category : #printing }
MetacelloProjectSpec >> configMethodBodyOn: aStream indent: indent fromShortCut: fromShortCut [
  | hasVersionString hasOperator hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
  hasClassName := self hasClassName.
  hasVersionString := self configHasVersionString.
  hasOperator := operator ~~ nil.
  hasProjectPackage := self hasRepository
    or: [ hasClassName & (self getFile ~~ nil) ].
  hasLoads := self loads ~~ nil.
  hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
  hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
  hasClassName
    ifTrue: [ 
      hasVersionString | hasOperator | hasProjectPackage | hasLoads
        ifTrue: [ 
          aStream
            cr;
            tab: indent + 1 ]
        ifFalse: [ aStream space ].
      aStream nextPutAll: 'className: ' , self className printString.
      hasVersionString | hasPreLoadDoIt | hasPostLoadDoIt | hasOperator
        | hasLoads | hasProjectPackage
        ifTrue: [ aStream nextPut: $; ] ].
  hasVersionString
    ifTrue: [ 
      | vs |
      hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPreLoadDoIt
        | hasPostLoadDoIt
        ifTrue: [ 
          aStream
            cr;
            tab: indent + 1 ]
        ifFalse: [ aStream space ].
      vs := self versionString.
      aStream nextPutAll: 'versionString: '.
      vs isSymbol
        ifTrue: [ aStream nextPut: $# ].
      aStream nextPutAll: vs asString printString.
      hasPreLoadDoIt | hasPostLoadDoIt | hasOperator | hasProjectPackage
        | hasLoads
        ifTrue: [ aStream nextPut: $; ] ].
  hasPreLoadDoIt
    ifTrue: [ 
      hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPreLoadDoIt
        ifTrue: [ 
          aStream
            cr;
            tab: indent + 1 ]
        ifFalse: [ aStream space ].
      aStream nextPutAll: 'preLoadDoIt: '.
      self preLoadDoIt value isSymbol
        ifTrue: [ 
          aStream
            nextPut: $#;
            nextPutAll: self preLoadDoIt value asString printString ]
        ifFalse: [ aStream nextPutAll: self preLoadDoIt value asString ].
      hasPostLoadDoIt | hasOperator | hasProjectPackage | hasLoads
        ifTrue: [ aStream nextPut: $; ] ].
  hasPostLoadDoIt
    ifTrue: [ 
      hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPostLoadDoIt
        ifTrue: [ 
          aStream
            cr;
            tab: indent + 1 ]
        ifFalse: [ aStream space ].
      aStream nextPutAll: 'postLoadDoIt: '.
      self postLoadDoIt value isSymbol
        ifTrue: [ 
          aStream
            nextPut: $#;
            nextPutAll: self postLoadDoIt value asString printString ]
        ifFalse: [ aStream nextPutAll: self postLoadDoIt value asString ].
      hasOperator | hasProjectPackage | hasLoads
        ifTrue: [ aStream nextPut: $; ] ].
  hasOperator
    ifTrue: [ 
      hasClassName | hasVersionString | hasProjectPackage | hasLoads | hasPreLoadDoIt
        | hasPostLoadDoIt
        ifTrue: [ 
          aStream
            cr;
            tab: indent + 1 ]
        ifFalse: [ aStream space ].
      aStream nextPutAll: 'operator: #' , self operator asString printString.
      hasProjectPackage | hasLoads
        ifTrue: [ aStream nextPut: $; ] ].
  hasLoads
    ifTrue: [ 
      hasClassName | hasVersionString | hasOperator | hasProjectPackage
        | hasPreLoadDoIt | hasPostLoadDoIt
        ifTrue: [ 
          aStream
            cr;
            tab: indent + 1 ]
        ifFalse: [ aStream space ].
      aStream nextPutAll: 'loads: #('.
      self loads do: [ :str | aStream nextPutAll: str printString , ' ' ].
      aStream nextPut: $).
      hasProjectPackage
        ifTrue: [ aStream nextPut: $; ] ].
  hasProjectPackage
    ifTrue: [ 
      | hasName hasRepo |
      hasRepo := self hasRepository.
      hasName := self file ~= self className.
      hasName
        ifTrue: [ 
          hasClassName | hasVersionString | hasOperator | hasLoads | hasPreLoadDoIt
            | hasPostLoadDoIt
            ifTrue: [ 
              aStream
                cr;
                tab: indent + 1 ]
            ifFalse: [ aStream space ].
          aStream nextPutAll: 'file: ' , self file printString.
          hasRepo
            ifTrue: [ aStream nextPut: $; ] ].
      hasRepo
        ifTrue: [ 
          | repos |
          repos := self repositories map values.
          repos size = 1
            ifTrue: [ 
              fromShortCut
                ifTrue: [ 
                  hasClassName | hasVersionString | hasOperator | hasLoads | hasPreLoadDoIt
                    | hasPostLoadDoIt | hasName
                    ifTrue: [ 
                      aStream
                        cr;
                        tab: indent + 1 ]
                    ifFalse: [ aStream space ] ]
                ifFalse: [ 
                  aStream
                    cr;
                    tab: indent + 1 ].
              repos first configMethodCascadeOn: aStream lastCascade: true ]
            ifFalse: [ 
              aStream cr.
              self repositories configMethodCascadeOn: aStream indent: indent ] ] ]
]

{ #category : #printing }
MetacelloProjectSpec >> configMethodOn: aStream indent: indent [

	aStream 
		tab: indent;
		nextPutAll: 'spec '; cr;
		tab: indent + 1;
		nextPutAll: 'name: ', self name printString, ';'.
	self configMethodBodyOn: aStream indent: indent.
	aStream nextPut: $.
]

{ #category : #printing }
MetacelloProjectSpec >> configShortCutMethodOn: aStream member: aMember indent: indent [
  | hasVersionString hasOperator hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
  hasClassName := self hasClassName.
  hasVersionString := self configHasVersionString.
  hasOperator := operator ~~ nil.
  hasProjectPackage := self hasRepository
    or: [ hasClassName & (self getFile ~~ nil or: [ className ~= self name ]) ].
  hasLoads := self loads ~~ nil.
  hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
  hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
  hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPreLoadDoIt
    | hasPostLoadDoIt
    ifTrue: [ 
      (aMember methodUpdateSelector == #'copy:'
        or: [ aMember methodUpdateSelector == #'with:' ])
        ifTrue: [ 
          aStream
            nextPutAll: 'with: [';
            cr ]
        ifFalse: [ 
          aStream
            nextPutAll: 'overrides: [';
            cr ].
      aStream
        tab: indent;
        nextPutAll: 'spec'.
      self configMethodBodyOn: aStream indent: indent fromShortCut: true.
      aStream nextPutAll: ' ]'.
      ^ self ].
  hasVersionString
    ifTrue: [ 
      | vs |
      vs := self versionString.
      aStream nextPutAll: 'with: '.
      vs isSymbol
        ifTrue: [ aStream nextPut: $# ].
      aStream nextPutAll: vs asString printString ]
]

{ #category : #private }
MetacelloProjectSpec >> constructClassName [
    ^ nil
]

{ #category : #querying }
MetacelloProjectSpec >> currentlyLoadedClassesInVersion [
    self versionOrNil ifNotNil: [ :vrsn | ^ vrsn currentlyLoadedClassesInVersion ].
    ^ #()
]

{ #category : #accessing }
MetacelloProjectSpec >> file [
    ^ self className
]

{ #category : #accessing }
MetacelloProjectSpec >> getClassName [
    "raw access to iv"

    ^ className
]

{ #category : #accessing }
MetacelloProjectSpec >> getFile [
    "raw access to iv"

    ^ nil
]

{ #category : #accessing }
MetacelloProjectSpec >> getOperator [

	^operator
]

{ #category : #querying }
MetacelloProjectSpec >> getPostLoadDoIt [

	^postLoadDoIt
]

{ #category : #querying }
MetacelloProjectSpec >> getPreLoadDoIt [

	^preLoadDoIt
]

{ #category : #accessing }
MetacelloProjectSpec >> getRepositories [
    "raw access to iv"

    ^ nil
]

{ #category : #printing }
MetacelloProjectSpec >> hasClassName [
    ^ className ~~ nil
]

{ #category : #testing }
MetacelloProjectSpec >> hasLoadConflicts: aMetacelloProjectSpec [
  ^ (self hasNoLoadConflicts: aMetacelloProjectSpec) not
]

{ #category : #scripting }
MetacelloProjectSpec >> hasNoLoadConflicts: aMetacelloProjectSpec [
  "same as compareEqual:, except if versionString and/or repositories are not 
	specified for either one, then there are not conflicts"

  ^ self className = aMetacelloProjectSpec className
    and: [ 
      (self compareVersionsEqual: aMetacelloProjectSpec)
        and: [ self operator == aMetacelloProjectSpec operator ] ]
]

{ #category : #testing }
MetacelloProjectSpec >> hasNonVersionStringField [
    | hasVersionString hasOperator hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
    hasClassName := self hasClassName.
    hasVersionString := self versionString ~~ nil.
    hasOperator := operator ~~ nil.
    hasProjectPackage := (self file ~~ nil and: [ hasClassName and: [ self className ~= self name ] ])
        or: [ self hasRepository ].
    hasLoads := self loads ~~ nil.
    hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
    hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
    ^ hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt
]

{ #category : #testing }
MetacelloProjectSpec >> hasRepository [
    ^ false
]

{ #category : #scripting }
MetacelloProjectSpec >> isLocked [
  ^ self registration locked
]

{ #category : #printing }
MetacelloProjectSpec >> label [

	^self name
]

{ #category : #loading }
MetacelloProjectSpec >> load [

	self subclassResponsibility
]

{ #category : #private }
MetacelloProjectSpec >> loadListForVersion: vrsn [ 

	^ (self loads == nil
			or: [self loads isEmpty])
		ifTrue: [vrsn spec defaultPackageNames]
		ifFalse: [self loads]
]

{ #category : #querying }
MetacelloProjectSpec >> loads [
	^ loads
]

{ #category : #accessing }
MetacelloProjectSpec >> loads: aCollection [

	aCollection setLoadsInMetacelloProject: self
]

{ #category : #construction }
MetacelloProjectSpec >> loads: anObject constructor: aVersionConstructor [
    aVersionConstructor loadsForProject: anObject
]

{ #category : #merging }
MetacelloProjectSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #name put: name.
	map at: #className put: className.
	map at: #versionString put: versionString.
	map at: #operator put: operator.
	map at: #loads put: loads.
	map at: #preLoadDoIt put: preLoadDoIt.
	map at: #postLoadDoIt put: postLoadDoIt.
	^map
]

{ #category : #scripting }
MetacelloProjectSpec >> mergeScriptLoads: aSpec [
    self shouldBeMutable.
    aSpec loads
        ifNotNil: [ :otherLoads | self loads ifNil: [ loads := otherLoads ] ifNotNil: [ loads := (loads , otherLoads) asSet asArray ] ].
    self loader: aSpec loader
]

{ #category : #scripting }
MetacelloProjectSpec >> metacelloRegistrationHash [
    "name className versionString operator loads preLoadDoIt postLoadDoIt"

    | hash |
    hash := String stringHash: name initialHash: 0.
    hash := String stringHash: self className initialHash: hash.
    hash := String stringHash: self versionString initialHash: hash.
    hash := String stringHash: self operator asString initialHash: hash.
    hash := String stringHash: self preLoadDoIt asString initialHash: hash.
    hash := String stringHash: self postLoadDoIt asString initialHash: hash.
    ^ hash bitXor: loads hash
]

{ #category : #querying }
MetacelloProjectSpec >> name [

	^name
]

{ #category : #accessing }
MetacelloProjectSpec >> name: aString [
    ((aString at: 1) isSeparator or: [ (aString at: aString size) isSeparator ])
        ifTrue: [ self error: 'Names are not allowed to have leading or trailing blanks: ' , aString printString ].
    self shouldBeMutable.
    name := aString
]

{ #category : #construction }
MetacelloProjectSpec >> name: aString constructor: aVersionConstructor [
    aVersionConstructor nameForProject: aString
]

{ #category : #querying }
MetacelloProjectSpec >> operator [

	operator == nil ifTrue: [ ^#>= ].
	^ operator
]

{ #category : #accessing }
MetacelloProjectSpec >> operator: anObject [
    " #= #~= #> #< #>= #<= #~> "

    self shouldBeMutable.
    operator := anObject
]

{ #category : #construction }
MetacelloProjectSpec >> operator: anObject constructor: aVersionConstructor [
    aVersionConstructor operatorForProject: anObject
]

{ #category : #querying }
MetacelloProjectSpec >> postLoadDoIt [

	^postLoadDoIt
]

{ #category : #accessing }
MetacelloProjectSpec >> postLoadDoIt: anObject [

	anObject setPostLoadDoItInMetacelloSpec: self
]

{ #category : #construction }
MetacelloProjectSpec >> postLoadDoIt: aSymbol constructor: aVersionConstructor [
    aVersionConstructor postLoadDoItForProject: aSymbol
]

{ #category : #querying }
MetacelloProjectSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : #accessing }
MetacelloProjectSpec >> preLoadDoIt: anObject [

	anObject setPreLoadDoItInMetacelloSpec: self
]

{ #category : #construction }
MetacelloProjectSpec >> preLoadDoIt: aSymbol constructor: aVersionConstructor [
    aVersionConstructor preLoadDoItForProject: aSymbol
]

{ #category : #visiting }
MetacelloProjectSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [
    projectBlock value: self
]

{ #category : #printing }
MetacelloProjectSpec >> projectLabel [
    ^ 'project'
]

{ #category : #querying }
MetacelloProjectSpec >> projectPackage [

	^nil
]

{ #category : #scripting }
MetacelloProjectSpec >> registration [
    ^ MetacelloProjectRegistration
        registrationForProjectSpec: self
        ifAbsent: [ :ignored |  ]
        ifPresent: [ :existing :new | existing ]
]

{ #category : #scripting }
MetacelloProjectSpec >> registrationsCompareEqual: aMetacelloProjectSpec [
    "name className versionString operator loads preLoadDoIt postLoadDoIt"

    ^ self className = aMetacelloProjectSpec className
        and: [ self versionString = aMetacelloProjectSpec versionString and: [ self operator == aMetacelloProjectSpec operator ] ]
]

{ #category : #private }
MetacelloProjectSpec >> resolveToLoadableSpec [

	^self copy
]

{ #category : #private }
MetacelloProjectSpec >> setLoads: aCollection [
    self shouldBeMutable.
    loads := aCollection
]

{ #category : #private }
MetacelloProjectSpec >> setName: aStringOrNil [
    self shouldBeMutable.
    name := aStringOrNil
]

{ #category : #accessing }
MetacelloProjectSpec >> setPostLoadDoIt: aSymbol [
    self shouldBeMutable.
    postLoadDoIt := aSymbol
]

{ #category : #accessing }
MetacelloProjectSpec >> setPreLoadDoIt: aSymbol [
    self shouldBeMutable.
    preLoadDoIt := aSymbol
]

{ #category : #scripting }
MetacelloProjectSpec >> unregisterProject [
    ^ MetacelloProjectRegistration
        registrationForProjectSpec: self
        ifAbsent: [  ]
        ifPresent: [ :existing :new | existing unregisterProject ]
]

{ #category : #querying }
MetacelloProjectSpec >> version [
    self subclassResponsibility
]

{ #category : #construction }
MetacelloProjectSpec >> version: anObject constructor: aVersionConstructor [
    aVersionConstructor versionForProject: anObject
]

{ #category : #querying }
MetacelloProjectSpec >> versionKey [
	"suitable for using as a unique key for the receiver's version in a dictionary"

	^ self version versionKey
]

{ #category : #querying }
MetacelloProjectSpec >> versionOrNil [

	^[ self version ] on: MetacelloVersionDoesNotExistError do: [:ex | ^nil ].
]

{ #category : #querying }
MetacelloProjectSpec >> versionString [
	^ versionString
]

{ #category : #accessing }
MetacelloProjectSpec >> versionString: anObject [
    self shouldBeMutable.
    versionString := anObject
]

{ #category : #construction }
MetacelloProjectSpec >> versionString: anObject constructor: aVersionConstructor [
    aVersionConstructor versionStringForProject: anObject
]
