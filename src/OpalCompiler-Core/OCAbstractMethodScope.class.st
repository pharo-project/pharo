"
I am an abstract superclass for Block and Method scopes
"
Class {
	#name : 'OCAbstractMethodScope',
	#superclass : 'OCAbstractScope',
	#instVars : [
		'tempVars',
		'copiedVars',
		'tempVector',
		'id',
		'tempVectorVar',
		'node'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'testing' }
OCAbstractMethodScope class >> isAbstract [
	^self = OCAbstractMethodScope
]

{ #category : 'temp vars - copying' }
OCAbstractMethodScope >> addCopyingTemp: arg1 [

	^ copiedVars at: arg1 name put: (arg1 createCopiedVariableFor: self)
]

{ #category : 'temp vars - copying' }
OCAbstractMethodScope >> addCopyingTempToAllScopesUpToDefTemp: arg1 [

	(self copiedVarNames includes: arg1 name) ifFalse: [
		self addCopyingTemp: arg1 ].
	self = arg1 scope ifTrue: [ ^ self ].
	nil.
	^ self outerScope addCopyingTempToAllScopesUpToDefTemp: arg1
]

{ #category : 'temp vars' }
OCAbstractMethodScope >> addTemp: arg1 [

	arg1 scope: self.
	^ tempVars at: arg1 name put: arg1
]

{ #category : 'temp vars - vector' }
OCAbstractMethodScope >> addVectorTemp: arg1 [

	| tmp2 |
	tmp2 := OCVectorTempVariable new.
	tmp2
		name: arg1 name;
		vectorName: self tempVectorName;
		scope: self;
		usage: arg1 usage;
		escaping: arg1 escaping.
	^ tempVector at: arg1 name put: tmp2 yourself
]

{ #category : 'temp vars' }
OCAbstractMethodScope >> allTempNames [

	^ self allTemps collect: [ :arg1 | arg1 name ]
]

{ #category : 'temp vars' }
OCAbstractMethodScope >> allTemps [

	^ Array streamContents: [ :arg1 |
		  self outerScope allTemps do: [ :arg2 |
			  (self localTempNames includes: arg2 name) ifFalse: [
				  arg1 nextPut: arg2 ] ].
		  arg1 nextPutAll: self localTemps ]
]

{ #category : 'temp vars - copying' }
OCAbstractMethodScope >> copiedVarNames [

	^ copiedVars keys
]

{ #category : 'temp vars - copying' }
OCAbstractMethodScope >> copiedVars [

	^ copiedVars
]

{ #category : 'temp vars - copying' }
OCAbstractMethodScope >> copyEscapingReads [

	self tempVars values
		select: [ :arg1 | arg1 isEscapingRead ]
		thenDo: [ :arg2 | self addCopyingTemp: arg2 ]
]

{ #category : 'lookup' }
OCAbstractMethodScope >> hasBindingThatBeginsWith: arg1 [

	(copiedVars hasBindingThatBeginsWith: arg1) ifTrue: [ ^ true ].
	nil.
	(tempVector hasBindingThatBeginsWith: arg1) ifTrue: [ ^ true ].
	nil.
	(tempVars hasBindingThatBeginsWith: arg1) ifTrue: [ ^ true ].
	nil.
	^ self outerScope hasBindingThatBeginsWith: arg1
]

{ #category : 'temp vars - vector' }
OCAbstractMethodScope >> hasTempVector [

	^ tempVector isNotEmpty
]

{ #category : 'accessing' }
OCAbstractMethodScope >> id: arg1 [

	id := arg1
]

{ #category : 'initialization' }
OCAbstractMethodScope >> initialize [

	tempVars := OrderedDictionary new.
	tempVector := OrderedDictionary new.
	copiedVars := OrderedDictionary new.
	id := 0
]

{ #category : 'temp vars' }
OCAbstractMethodScope >> localTempNames [

	^ self localTemps collect: [ :arg1 | arg1 name ]
]

{ #category : 'temp vars' }
OCAbstractMethodScope >> localTemps [

	^ Array streamContents: [ :arg1 |
		  copiedVars valuesDo: [ :arg2 |
			  arg2 isStoringTempVector ifTrue: [
				  arg2 originalVar scope tempVector do: [ :arg3 |
					  arg1 nextPut: arg3 ] ] ].
		  arg1 nextPutAll: tempVars values ]
]

{ #category : 'lookup' }
OCAbstractMethodScope >> lookupDefiningContextForVariable: arg1 startingFrom: arg2 [

	^ self = arg1 scope
		  ifTrue: [ arg2 ]
		  ifFalse: [
			  self outerScope
				  lookupDefiningContextForVariable: arg1
				  startingFrom: (self nextOuterScopeContextOf: arg2) ]
]

{ #category : 'lookup' }
OCAbstractMethodScope >> lookupVar: arg1 [

	copiedVars at: arg1 ifPresent: [ :arg2 | ^ arg2 ].
	tempVector at: arg1 ifPresent: [ :arg3 | ^ arg3 ].
	tempVars at: arg1 ifPresent: [ :arg4 | ^ arg4 ].
	arg1 = self tempVectorName ifTrue: [ ^ self tempVectorVar ].
	nil.
	^ super lookupVar: arg1
]

{ #category : 'scope' }
OCAbstractMethodScope >> methodScope [

	^ self outerScope methodScope
]

{ #category : 'temp vars - vector' }
OCAbstractMethodScope >> moveEscapingWritesToTempVector [

	self tempVars values
		select: [ :arg1 | arg1 isEscapingWrite ]
		thenDo: [ :arg2 | self moveToVectorTemp: arg2 ]
]

{ #category : 'temp vars - vector' }
OCAbstractMethodScope >> moveToVectorTemp: arg1 [

	self
		addVectorTemp: arg1;
		removeTemp: arg1
]

{ #category : 'scope' }
OCAbstractMethodScope >> newBlockScope: arg1 [

	| tmp2 |
	tmp2 := OCBlockScope new.
	tmp2
		outerScope: self;
		id: arg1.
	^ tmp2 yourself
]

{ #category : 'scope' }
OCAbstractMethodScope >> newOptimizedBlockScope: arg1 [

	| tmp2 |
	tmp2 := OCOptimizedBlockScope new.
	tmp2
		outerScope: self;
		id: arg1.
	^ tmp2 yourself
]

{ #category : 'lookup' }
OCAbstractMethodScope >> nextOuterScopeContextOf: arg1 [

	^ arg1
]

{ #category : 'accessing' }
OCAbstractMethodScope >> node [

	^ node
]

{ #category : 'accessing' }
OCAbstractMethodScope >> node: arg1 [

	node := arg1
]

{ #category : 'scope' }
OCAbstractMethodScope >> outerNotOptimizedScope [

	
]

{ #category : 'scope' }
OCAbstractMethodScope >> popScope [

	^ self outerScope
]

{ #category : 'temp vars' }
OCAbstractMethodScope >> removeTemp: arg1 [

	tempVars removeKey: arg1 name
]

{ #category : 'temp vars - copying' }
OCAbstractMethodScope >> setCopyingTempToAllScopesUpToDefTemp: arg1 to: arg2 from: arg3 [

	self = arg1 scope ifTrue: [
		^ arg1 writeFromLocalContext: arg3 put: arg2 ].
	nil.
	(self lookupVar: arg1 name) writeFromLocalContext: arg3 put: arg2.
	^ self outerScope
		  setCopyingTempToAllScopesUpToDefTemp: arg1
		  to: arg2
		  from: (self nextOuterScopeContextOf: arg3)
]

{ #category : 'temp vars' }
OCAbstractMethodScope >> tempVarNames [

	^ tempVars keys
]

{ #category : 'temp vars' }
OCAbstractMethodScope >> tempVarNamesWithoutArguments [

	^ tempVars values
		  reject: [ :arg1 | arg1 isArgumentVariable ]
		  thenCollect: [ :arg2 | arg2 name ]
]

{ #category : 'temp vars' }
OCAbstractMethodScope >> tempVars [

	^ tempVars
]

{ #category : 'temp vars - vector' }
OCAbstractMethodScope >> tempVector [

	^ tempVector
]

{ #category : 'temp vars - vector' }
OCAbstractMethodScope >> tempVectorName [

	^ '0vector' , id asString
]

{ #category : 'temp vars - vector' }
OCAbstractMethodScope >> tempVectorVar [

	^ tempVectorVar
		  ifNil: [
			  | tmp2 |
			  tmp2 := TemporaryVariable new.
			  tmp2
				  name: self tempVectorName;
				  scope: self.
			  tempVectorVar := tmp2 yourself ]
		  ifNotNil: [ tempVectorVar ]
]

{ #category : 'temp vars - vector' }
OCAbstractMethodScope >> tempVectorVarNames [

	^ (self tempVector collect: [ :arg1 | arg1 name ]) asArray
]
