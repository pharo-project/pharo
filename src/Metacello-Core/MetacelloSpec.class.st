Class {
	#name : #MetacelloSpec,
	#superclass : #Object,
	#instVars : [
		'project',
		'loader',
		'mutable'
	],
	#category : #Metacello-Core-Specs
}

{ #category : #'instance creation' }
MetacelloSpec class >> for: aVersionMap [

	^(self platformClass new)
		for: aVersionMap;
		yourself
]

{ #category : #accessing }
MetacelloSpec class >> platformClass [

	^self
]

{ #category : #merging }
MetacelloSpec >> aboutToCopy [
]

{ #category : #'spec creation' }
MetacelloSpec >> addMember [

	^MetacelloAddMemberSpec for: self project
]

{ #category : #querying }
MetacelloSpec >> answers [

	^#()
]

{ #category : #printing }
MetacelloSpec >> configMethodOn: aStream [

	self configMethodOn: aStream indent: 0
]

{ #category : #printing }
MetacelloSpec >> configMethodOn: aStream indent: indent [

	self subclassResponsibility
]

{ #category : #'spec creation' }
MetacelloSpec >> copyMember [

	^MetacelloCopyMemberSpec for: self project
]

{ #category : #mutability }
MetacelloSpec >> copyOnWrite: aBlock [
    "assume that only registered projects are immutable ... otherwise you'll get an error"

    | copy |
    copy := self copy.
    aBlock value: copy.
    ^ copy
]

{ #category : #doits }
MetacelloSpec >> doItBlock: selector [

	selector == nil ifTrue: [ ^nil ].
	selector numArgs = 0
		ifTrue: [ ^[ self project configuration perform: selector ] ].
	selector numArgs = 1
		ifTrue: [ ^[:aLoader | self project configuration perform: selector with: aLoader ] ].
	selector numArgs = 2
		ifTrue: [ ^[:aLoader :pkgSpec | self project configuration perform: selector with: aLoader with: pkgSpec ] ].
	^nil
]

{ #category : #initialization }
MetacelloSpec >> for: aProject [
    self shouldBeMutable.
    project := aProject
]

{ #category : #mutability }
MetacelloSpec >> immutable [
    mutable := false
]

{ #category : #mutability }
MetacelloSpec >> isMutable [
    mutable ifNil: [ ^ true ].
    ^ mutable
]

{ #category : #printing }
MetacelloSpec >> label [

	^self printString
]

{ #category : #accessing }
MetacelloSpec >> loader [

	loader == nil 
		ifTrue: [ | prjctLoader |
			(prjctLoader := self project loader) == nil
				ifTrue: [ self loader: (self project loaderClass on: self) ]
				ifFalse: [ self loader: prjctLoader ]].
	^loader
]

{ #category : #accessing }
MetacelloSpec >> loader: aLoader [
    "We're interested in propogating the loader state, _except_ for the spec"

    self shouldBeMutable.
    loader := aLoader copy.
    loader spec: self
]

{ #category : #importing }
MetacelloSpec >> mergeImportLoads: aLoadList [
    self error: 'import: can only be used with baseline project specs'
]

{ #category : #merging }
MetacelloSpec >> mergeMap [

	^Dictionary new.
]

{ #category : #'spec creation' }
MetacelloSpec >> mergeMember [

	^MetacelloMergeMemberSpec for: self project
]

{ #category : #merging }
MetacelloSpec >> mergeSpec: aSpec [
    | newSpec nonOverridable |
    self validateMergeForSpec: aSpec.
    newSpec := self copy.
    nonOverridable := self nonOverridable.
    aSpec mergeMap
        keysAndValuesDo: [ :key :value | 
            (nonOverridable includes: key)
                ifFalse: [ 
                    value ~~ nil
                        ifTrue: [ newSpec instVarNamed: key asString put: value ] ] ].
    ^ newSpec
]

{ #category : #mutability }
MetacelloSpec >> mutable [
    mutable := true
]

{ #category : #merging }
MetacelloSpec >> nonOverridable [

	^#()
]

{ #category : #copying }
MetacelloSpec >> postCopy [
    super postCopy.
    mutable := nil.
    loader ~~ nil
        ifTrue: [ self loader: loader ].
]

{ #category : #doits }
MetacelloSpec >> postLoadDoIt [
	"noop unless non-nil value returned"

	^nil
]

{ #category : #doits }
MetacelloSpec >> postLoadDoItBlock [

	^self doItBlock: self postLoadDoIt value
]

{ #category : #doits }
MetacelloSpec >> preLoadDoIt [
	"noop unless non-nil value returned"
	
	^nil
]

{ #category : #doits }
MetacelloSpec >> preLoadDoItBlock [

	^self doItBlock: self preLoadDoIt value
]

{ #category : #printing }
MetacelloSpec >> printOn: aStream [

	self configMethodOn: aStream indent: 0
]

{ #category : #querying }
MetacelloSpec >> project [

	^project
]

{ #category : #'spec creation' }
MetacelloSpec >> removeMember [

	^MetacelloRemoveMemberSpec for: self project
]

{ #category : #mutability }
MetacelloSpec >> shouldBeMutable [
    self isMutable
        ifTrue: [ ^ self ].
    self error: 'Not allowed to modify an immutable object'
]

{ #category : #merging }
MetacelloSpec >> validateMergeForSpec: aSpec [
    aSpec class = self class
        ifFalse: [ 
            self
                error:
                    'The project spec ' , self name printString , ' in project ' , self project label , ' has incompatible specs. '
                        , aSpec class name asString , ' and ' , self class name asString , ' are not compatible.' ]
]
