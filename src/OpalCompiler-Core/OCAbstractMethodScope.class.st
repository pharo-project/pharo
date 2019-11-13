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
		'id',
		'tempVectorVar'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

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
OCAbstractMethodScope >> addCopyingTempToAllScopesUpToDefTemp: aVar [

	(self hasCopyingTempNamed: aVar name) ifFalse: [self addCopyingTemp: aVar].
	self = aVar scope ifTrue: [^ self].
	^ self outerScope addCopyingTempToAllScopesUpToDefTemp: aVar.
				
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

{ #category : #'temp vars' }
OCAbstractMethodScope >> indexFromIRForVarNamed: aName [
	^self outerNotOptimizedScope node irInstruction indexForVarNamed: aName
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
						ifTrue: [ var originalVar scope tempVector
								do: [ :tempVectorVars | str nextPut: tempVectorVars ] ] ].
			str nextPutAll: self tempVars ]
]

{ #category : #lookup }
OCAbstractMethodScope >> lookupDefiningContextForVariable: var startingFrom: aContext [
	"Is this the definition context for var? If it not, we look in the outer context using the corresponding outer scope. If found, we return the context"
	
	^self = var scope
		ifFalse: [ self outerScope lookupDefiningContextForVariable: var startingFrom: (self nextOuterScopeContextOf: aContext) ]
		ifTrue: [ aContext ]
]

{ #category : #lookup }
OCAbstractMethodScope >> lookupVar: name [
	
	copiedVars at: name ifPresent: [:v | ^ v].
	tempVector at: name ifPresent: [:v | ^ v].
	tempVars at: name ifPresent: [:v | ^ v].
	name = 'thisContext' ifTrue: [^ thisContextVar].
	name = self tempVectorName ifTrue: [ ^ self tempVectorVar ].
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

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> setCopyingTempToAllScopesUpToDefTemp: aVar to: aValue from: aContext [
	"we need to update all the copies if we change the value of a copied temp"

	self = aVar scope
		ifTrue: [ ^ aVar writeFromLocalContext: aContext put: aValue ].
	(self lookupVar: aVar name) writeFromLocalContext: aContext put: aValue.
	^self outerScope
		setCopyingTempToAllScopesUpToDefTemp: aVar
		to: aValue
		from: (self nextOuterScopeContextOf: aContext)
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

{ #category : #'temp vars - vector' }
OCAbstractMethodScope >> tempVectorVar [
	^ tempVectorVar ifNil: [ tempVectorVar := OCTempVariable new
			name: self tempVectorName;
			scope: self;
			yourself]
]
