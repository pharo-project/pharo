"
I am an abstract superclass for Block and Method scopes
"
Class {
	#name : #OCAbstractMethodScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'thisContextVar',
		'tempVars',
		'copiedVars',
		'tempVector',
		'id'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> addCopiedTempToAllScopesUpToDefVector: aVariable [
		
	(self hasCopyingTempNamed: aVariable name)
		ifFalse: [self addCopyingTemp: aVariable ].
	self tempVectorName = aVariable name ifTrue: [^ self].
	^ self outerScope addCopiedTempToAllScopesUpToDefVector: aVariable.
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> addCopyingTemp: aTempVar [
	copiedVars add: (OCCopyingTempVariable new
			originalVar: aTempVar originalVar;
			name: aTempVar name;
			escaping: aTempVar escaping;
			usage: aTempVar usage;
			scope: self;
			yourself).
	^copiedVars
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> addCopyingTempNamed: name [
	copiedVars add: (OCCopyingTempVariable new
			name: name;
			scope: self;
			yourself).
	^copiedVars
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> addCopyingTempToAllScopesUpToDefTemp: aVar [

	(self hasCopyingTempNamed: aVar name) ifFalse: [self addCopyingTemp: aVar].
	tempVars at: aVar name ifPresent: [:v | ^ self].
	^ self outerScope addCopyingTempToAllScopesUpToDefTemp: aVar.
				
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> addCopyingTempToAllScopesUpToDefVector: aName [
		
	(self hasCopyingTempNamed: aName) ifFalse: [self addCopyingTempNamed: aName].
	self tempVectorName = aName ifTrue: [^ self].
	^ self outerScope addCopyingTempToAllScopesUpToDefVector: aName.
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> addTemp: name [
	^self addTemp: OCTempVariable new withName: name
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> addTemp: aTempVariable withName: aString [
	^ tempVars
		at: aString
		put: (aTempVariable
				name: aString;
				scope: self;
				yourself)
]

{ #category : #'temp vars - vector' }
OCAbstractMethodScope >> addVectorTemp: aTemp [
	^ tempVector 
		at:  aTemp name
		put: (OCVectorTempVariable new
			name: aTemp name;
			vectorName: self tempVectorName;
			scope: self;
			usage: aTemp usage;
			escaping: aTemp escaping;
			yourself)
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> allTempNames [
	^self allTemps collect: [: each | each name]
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> allTemps [
	"return all temps defined, even the ones in the outer scope that are not used in the current.
	 This includes the arguments. We do not need to care about shadowed temps as temp shadowing is not allowed."

	^ Array streamContents: [ :str | 
			self outerScope allTemps
				do: [ :var | 
					(self localTempNames includes: var name)
						ifFalse: [ str nextPut: var ] ].
			str nextPutAll: self localTemps ]
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> copiedVars [
	^ copiedVars
]

{ #category : #lookup }
OCAbstractMethodScope >> findVariable: aBlock ifNone: aNotFound [

	(aBlock value: thisContextVar) ifTrue: [^ thisContextVar].
	self allTemps
		detect: [ :var | aBlock value: var ]
		ifFound: [ :var | ^ var ].
	
	outerScope ifNotNil: [
		^  outerScope findVariable: aBlock ifNone: aNotFound ]
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> hasCopyingTempNamed: name [
	^self copiedVars anySatisfy: [:each | each name = name]
]

{ #category : #'temp vars - vector' }
OCAbstractMethodScope >> hasTempVector [
	^ tempVector isNotEmpty
]

{ #category : #accessing }
OCAbstractMethodScope >> id: int [
	id := int
]

{ #category : #initialization }
OCAbstractMethodScope >> initialize [

	tempVars :=  OrderedDictionary new.
	tempVector  := Dictionary new.
	copiedVars := OCKeyedSet keyBlock: [:var | var name].
	id := 0.
	
	thisContextVar := OCThisContextVariable new
		scope: self; yourself.

]

{ #category : #'temp vars' }
OCAbstractMethodScope >> localTempNames [
	^self localTemps collect: [:each | each name]
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> localTemps [
	"all temps accessed in the context... for tempVectors, it takes all the vars even those not used here"

	^ Array streamContents: [ :str | 
			self copiedVars
				do: [ :var | 
					var isStoringTempVector
						ifTrue: [ var tempVectorForTempStoringIt
								do: [ :tempVectorVars | str nextPut: tempVectorVars ] ] ].
			str nextPutAll: self tempVars ]
]

{ #category : #lookup }
OCAbstractMethodScope >> lookupDefiningContextFor: name startingFrom: aContext [
	"We lookup a variable in a context. If it not in this context, we look in the outer context using the corresponding outer scope. If found, we return the context"
	
	self
		variableNamed: name
		ifAbsent: [ ^self outerScope lookupDefiningContextFor: name startingFrom: (self nextOuterScopeContextOf: aContext)].
	^aContext
]

{ #category : #lookup }
OCAbstractMethodScope >> lookupTempVector: name inContext: aContext [
	"Similar to lookupAndReadVar:inContext: but looks for a temp vector. If we don't find the temp vector (because it was nilled or not created), we lookup in the outer context with the corresponding outer scope"
	
	| variable theVector |
	variable := self
		variableNamed: name
		ifAbsent: [^ self outerScope lookupTempVector: name inContext: aContext outerContext ].
	theVector := variable readFromContext: aContext.
	theVector ifNil: [ ^ self outerScope lookupTempVector: name inContext: aContext outerContext ].
	^ { variable . theVector }
]

{ #category : #lookup }
OCAbstractMethodScope >> lookupVar: name [
	copiedVars at: name ifPresent: [:v | ^ v].
	tempVector  at: name ifPresent: [:v | ^ v].
	tempVars at: name ifPresent: [:v | ^ v].
	name = 'thisContext' ifTrue: [^ thisContextVar].
	^self outerScope lookupVar: name
	
]

{ #category : #lookup }
OCAbstractMethodScope >> lookupVarForDeclaration: name [
	tempVars at: name ifPresent: [:v | ^ v].
	name = 'thisContext' ifTrue: [^ thisContextVar].
	^self outerScope lookupVarForDeclaration: name
]

{ #category : #scope }
OCAbstractMethodScope >> methodScope [

	^ self outerScope methodScope
]

{ #category : #'temp vars - vector' }
OCAbstractMethodScope >> moveToVectorTemp: aTempVar [

	self addVectorTemp: aTempVar.
	self removeTemp: aTempVar.	
]

{ #category : #scope }
OCAbstractMethodScope >> newBlockScope: int [
	^ OCBlockScope new outerScope: self; id: int; yourself.
]

{ #category : #scope }
OCAbstractMethodScope >> newOptimizedBlockScope: int [
	^ OCOptimizedBlockScope new 
			outerScope: self; 
			id: int;
			yourself.
]

{ #category : #lookup }
OCAbstractMethodScope >> nextOuterScopeContextOf: aContext [
	^aContext
]

{ #category : #scope }
OCAbstractMethodScope >> outerNotOptimizedScope [
	^self
]

{ #category : #scope }
OCAbstractMethodScope >> popScope [
	"Propogate free var usages to their outer vars, then return outer scope"

	^ self outerScope
]

{ #category : #printing }
OCAbstractMethodScope >> printOn: stream [

	super printOn: stream.
	stream space.
	self scopeLevel printOn: stream.

]

{ #category : #'temp vars' }
OCAbstractMethodScope >> removeTemp: tempVar [

	tempVars removeKey: tempVar name
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> tempVarNames [

	^ self tempVars collect: [:each| each name]
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> tempVars [

	^ tempVars
]

{ #category : #'temp vars - vector' }
OCAbstractMethodScope >> tempVector [
	^ tempVector 
]

{ #category : #'temp vars - vector' }
OCAbstractMethodScope >> tempVectorName [
	"the name of the tempVector is not a valid name of a temp variable
	 This way we avoid name clashes "
	^'0vector', id asString
]

{ #category : #lookup }
OCAbstractMethodScope >> variableNamed: name ifAbsent: aBlock [
	copiedVars at: name ifPresent: [:v | ^ v].
	tempVector  at: name ifPresent: [:v | ^ v].
	tempVars at: name ifPresent: [:v | ^ v].
	name = 'thisContext' ifTrue: [^ thisContextVar].
	
	^ aBlock value.
]
