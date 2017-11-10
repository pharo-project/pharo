Class {
	#name : #MetacelloAbstractPackageSpec,
	#superclass : #MetacelloSpec,
	#instVars : [
		'name',
		'requires',
		'includes',
		'answers'
	],
	#category : #Metacello-Core-Specs
}

{ #category : #adding }
MetacelloAbstractPackageSpec >> addToMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec addMember 
			name: self name;
			spec: self;
			yourself)
]

{ #category : #querying }
MetacelloAbstractPackageSpec >> answers [

	answers == nil ifTrue: [ answers := #() ].
	^answers
]

{ #category : #accessing }
MetacelloAbstractPackageSpec >> answers: aListOfPairs [

	self setAnswers: aListOfPairs
]

{ #category : #printing }
MetacelloAbstractPackageSpec >> configMethodBodyOn: aStream hasName: hasName cascading: cascading indent: indent [

	| hasCascading hasRequires hasIncludes hasAnswers |
	hasCascading := cascading.
	hasRequires := self requires isEmpty not.
	hasIncludes := self includes isEmpty not.
	hasAnswers := self answers isEmpty not.
	hasRequires
		ifTrue: [ 
			hasName | hasIncludes | hasAnswers | hasCascading
				ifTrue: [ aStream cr; tab: indent ].
			aStream nextPutAll: 'requires: #('.
			self requires do: [:str | aStream nextPutAll: str printString, ' ' ].
			hasIncludes | hasAnswers | hasCascading
				ifTrue: [ aStream nextPutAll: ');' ]
				ifFalse: [ aStream nextPut: $) ]].
	hasIncludes
		ifTrue: [ 
			hasName | hasRequires | hasAnswers | hasCascading
				ifTrue: [ aStream cr; tab: indent ].
			aStream nextPutAll: 'includes: #('.
			self includes do: [:str | aStream nextPutAll: str printString, ' ' ].
			hasAnswers | hasCascading
				ifTrue: [ aStream nextPutAll: ');' ]
				ifFalse: [ aStream nextPut: $) ]].
	hasAnswers
		ifTrue: [ 
			hasName | hasRequires | hasIncludes | hasCascading
				ifTrue: [ aStream cr; tab: indent ].
			aStream nextPutAll: 'supplyingAnswers: #( '.
			self answers do: [:ar | 
				aStream nextPutAll: '#( '.
				ar do: [:val | 
					(val isString or: [ val isNumber or: [ val isSymbol or: [ val isCharacter ]]])
						ifTrue: [  aStream nextPutAll: val printString, ' ' ].
					val == true
						ifTrue: [  aStream nextPutAll: 'true ' ].
					val == false
						ifTrue: [  aStream nextPutAll: 'false ' ]].
				aStream nextPutAll: ') ' ].
			hasCascading
				ifTrue: [ aStream nextPutAll: ');' ]
				ifFalse: [ aStream nextPut: $) ]].
]

{ #category : #printing }
MetacelloAbstractPackageSpec >> configMethodCascadeOn: aStream member: aMember last: lastCascade indent: indent [

	self subclassResponsibility
]

{ #category : #printing }
MetacelloAbstractPackageSpec >> configMethodOn: aStream for: aValue selector: selector cascading: cascading cascade: cascade indent: indent [

	| valuePrintString |
	aValue == nil ifTrue: [ ^self ].
	cascading ifTrue: [ aStream cr; tab: indent ].
	valuePrintString := aValue value isSymbol
		ifTrue: [ '#' , aValue value asString printString ]
		ifFalse: [ aValue value printString ].
	aStream  nextPutAll: selector, valuePrintString.
	cascade ifTrue: [ aStream nextPut: $; ]
]

{ #category : #testing }
MetacelloAbstractPackageSpec >> hasRepository [
    ^ false
]

{ #category : #private }
MetacelloAbstractPackageSpec >> includeSpecNamesForPackageOrdering: aVersionSpec [
  ^ self specs: self includesForPackageOrdering forPackageOrdering: aVersionSpec
]

{ #category : #querying }
MetacelloAbstractPackageSpec >> includes [

	includes == nil ifTrue: [ includes := #() ].
	^includes
]

{ #category : #accessing }
MetacelloAbstractPackageSpec >> includes: aCollection [

	aCollection setIncludesInMetacelloPackage: self
]

{ #category : #private }
MetacelloAbstractPackageSpec >> includesForPackageOrdering [
  ^ #()
]

{ #category : #printing }
MetacelloAbstractPackageSpec >> label [

	^self name
]

{ #category : #merging }
MetacelloAbstractPackageSpec >> mergeIntoMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec mergeMember 
			name: self name;
			spec: self;
			yourself)
]

{ #category : #merging }
MetacelloAbstractPackageSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #requires put: requires.
	map at: #includes put: includes.
	map at: #answers put: answers.
	^map
]

{ #category : #merging }
MetacelloAbstractPackageSpec >> mergeSpec: anotherSpec [

	| newSpec map anotherRequires anotherIncludes anotherAnswers |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	anotherSpec name ~~ nil 
		ifTrue: [ newSpec name: anotherSpec name ].
	(anotherRequires := map at: #requires) ~~ nil
		ifTrue: [ newSpec setRequires: self requires, anotherRequires ].
	(anotherIncludes := map at: #includes) ~~ nil
		ifTrue: [ newSpec setIncludes: self includes, anotherIncludes ].
	(anotherAnswers := map at: #answers) ~~ nil
		ifTrue: [ newSpec setAnswers: self answers, anotherAnswers ].
	^newSpec
]

{ #category : #querying }
MetacelloAbstractPackageSpec >> name [

	^name
]

{ #category : #accessing }
MetacelloAbstractPackageSpec >> name: aString [
    ((aString at: 1) isSeparator or: [ (aString at: aString size) isSeparator ])
        ifTrue: [ self error: 'Names are not allowed to have leading or trailing blanks: ' , aString printString ].
    name := aString
]

{ #category : #merging }
MetacelloAbstractPackageSpec >> nonOverridable [

	^#( includes requires answers )
]

{ #category : #copying }
MetacelloAbstractPackageSpec >> postCopy [

	super postCopy.
	requires := requires copy.
	includes := includes copy.
	answers := answers copy.
]

{ #category : #visiting }
MetacelloAbstractPackageSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	self subclassResponsibility
]

{ #category : #accessing }
MetacelloAbstractPackageSpec >> referencedSpec [

	^self
]

{ #category : #removing }
MetacelloAbstractPackageSpec >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec removeMember 
			name: self name;
			spec: self;
			yourself)
]

{ #category : #private }
MetacelloAbstractPackageSpec >> requiredSpecNamesForPackageOrdering: aVersionSpec [
  ^ (self requiredSpecsForPackageOrdering: aVersionSpec)
    collect: [ :spec | spec name ]
]

{ #category : #private }
MetacelloAbstractPackageSpec >> requiredSpecsForPackageOrdering: aVersionSpec [
  ^ self specs: self requires forPackageOrdering: aVersionSpec
]

{ #category : #querying }
MetacelloAbstractPackageSpec >> requires [

	requires == nil ifTrue: [ requires := #() ].
	^requires
]

{ #category : #accessing }
MetacelloAbstractPackageSpec >> requires: aCollection [

	aCollection setRequiresInMetacelloPackage: self
]

{ #category : #private }
MetacelloAbstractPackageSpec >> requiresSpecsForPackageOrdering: aVersionSpec [
  ^ {self}
]

{ #category : #private }
MetacelloAbstractPackageSpec >> resolveSpecsForPackageOrdering: aVersionSpec [
  ^ {self}
]

{ #category : #private }
MetacelloAbstractPackageSpec >> setAnswers: aCollection [

	answers := aCollection
]

{ #category : #private }
MetacelloAbstractPackageSpec >> setIncludes: aCollection [

	includes := aCollection
]

{ #category : #private }
MetacelloAbstractPackageSpec >> setRequires: aCollection [

	requires := aCollection
]

{ #category : #private }
MetacelloAbstractPackageSpec >> specs: specNames forPackageOrdering: aVersionSpec [
  | packageOrProjectSpecs |
  packageOrProjectSpecs := Set new.
  specNames
    do: [ :specName | 
      | spec |
      "get project/group/package specs"
      spec := aVersionSpec
        packageNamed: specName
        ifAbsent: [ 
          MetacelloNameNotDefinedError
            signal:
              'project group, or package named: ' , specName printString
                ,
                  ' not found when used in requires: or includes: field of package: '
                , self name printString , ' for version: '
                , aVersionSpec versionString , ' of '
                , aVersionSpec projectLabel , '.'.
          nil	"return nil if resumed" ].
      spec
        ifNotNil: [ 
          packageOrProjectSpecs
            addAll: (spec resolveSpecsForPackageOrdering: aVersionSpec) ] ].
  ^ packageOrProjectSpecs
]
