"
I am an abstract superclass for Block and Method scopes
"
Class {
	#name : #OCAbstractMethodScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'tempVars',
		'copiedVars',
		'tempVector',
		'id',
		'tempVectorVar',
		'node'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #testing }
OCAbstractMethodScope class >> isAbstract [ 
	^self = OCAbstractMethodScope 
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> addCopyingTemp: aTempVar [
	^copiedVars at: aTempVar name put: (aTempVar createCopiedVariableFor: self)
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> addCopyingTempToAllScopesUpToDefTemp: aVar [

	(self copiedVarNames includes: aVar name) ifFalse: [self addCopyingTemp: aVar].
	self = aVar scope ifTrue: [^ self].
	^ self outerScope addCopyingTempToAllScopesUpToDefTemp: aVar.
				
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> addTemp: aTempVar [	
	aTempVar scope: self.
	^ tempVars
		at: aTempVar name
		put: aTempVar
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
OCAbstractMethodScope >> copiedVarNames [
	^ copiedVars keys
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> copiedVars [
	^ copiedVars
]

{ #category : #'temp vars - copying' }
OCAbstractMethodScope >> copyEscapingReads [

	self tempVars values 
		select: [ :each | each isEscapingRead ]
		thenDo: [ :each | self addCopyingTemp: each ]
]

{ #category : #lookup }
OCAbstractMethodScope >> hasBindingThatBeginsWith: aString [
	"check weather there are any temporaries defined that start with aString"
	(copiedVars hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	(tempVector hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	(tempVars hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	
	^ self outerScope hasBindingThatBeginsWith: aString
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
	tempVector  := OrderedDictionary new.
	copiedVars := OrderedDictionary new.
	id := 0
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> localTempNames [
	^self localTemps collect: [:each | each name]
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> localTemps [
	"all temps accessed in the context... for tempVectors, it takes all the vars even those not used here"

	^ Array streamContents: [ :str | 
			copiedVars valuesDo: [ :var | 
					var isStoringTempVector
						ifTrue: [ var originalVar scope tempVector
								do: [ :tempVectorVars | str nextPut: tempVectorVars ] ] ].
			str nextPutAll: tempVars values]
]

{ #category : #lookup }
OCAbstractMethodScope >> lookupDefiningContextForVariable: var startingFrom: aContext [
	"Is this the definition context for var? If it not, we look in the outer context using the corresponding outer scope. If found, we return the context"
	
	^self = var scope
		ifFalse: [ self outerScope lookupDefiningContextForVariable: var startingFrom: (self nextOuterScopeContextOf: aContext) ]
		ifTrue: [ aContext ]
]

{ #category : #lookup }
OCAbstractMethodScope >> lookupVar: name declare: aBoolean [

	copiedVars at: name ifPresent: [:v | ^ v].
	tempVector at: name ifPresent: [:v | ^ v].
	tempVars at: name ifPresent: [:v | ^ v].
	name = self tempVectorName ifTrue: [ ^ self tempVectorVar ].
	^self outerScope lookupVar: name declare: aBoolean
	
]

{ #category : #scope }
OCAbstractMethodScope >> methodScope [

	^ self outerScope methodScope
]

{ #category : #'temp vars - vector' }
OCAbstractMethodScope >> moveEscapingWritesToTempVector [

	self tempVars values
		select: [ :each | each isEscapingWrite ]
		thenDo: [ :each | self moveToVectorTemp: each ]
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

{ #category : #accessing }
OCAbstractMethodScope >> node [
	^node
]

{ #category : #accessing }
OCAbstractMethodScope >> node: aNode [
	node := aNode
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

{ #category : #printing }
OCAbstractMethodScope >> scopeLevel [
	"For debugging we print a counter for all Method and Block scopes"
	^ outerScope scopeLevel + 1
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

	^ tempVars keys
]

{ #category : #'temp vars' }
OCAbstractMethodScope >> tempVarNamesWithoutArguments [

	^ tempVars values reject: [ :each | each isArgumentVariable ] thenCollect: [ :each | each name ]
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
	^ tempVectorVar ifNil: [ tempVectorVar := TemporaryVariable new
			name: self tempVectorName;
			scope: self;
			yourself]
]

{ #category : #'temp vars - vector' }
OCAbstractMethodScope >> tempVectorVarNames [
	"As other users are taking the values directly from the tempVector Dictionary, 
	it is important that the collect is performed like this. As in this way it has always the order dependending on the hash of the String (that is constant). 
	If done with collect:as: it has the order of the Dictionary that is almost random (changes from image run to image run)"
	^ (self tempVector collect: [:each| each name]) asArray
]
