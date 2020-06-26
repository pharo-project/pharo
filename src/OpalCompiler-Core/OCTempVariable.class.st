"
I model temp variables. With Closures, there are two kinds: Copying and those that are stored in a so called temp vector, a heap allocated array that itself is stored in a copying temp variable.
"
Class {
	#name : #OCTempVariable,
	#superclass : #OCAbstractLocalVariableTOMERGE,
	#instVars : [
		'escaping',
		'index'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCTempVariable class >> semanticNodeClass [

	^RBTemporaryNode 
]

{ #category : #comparing }
OCTempVariable >> = aTempVar [

	^aTempVar class = self class 
		and: [aTempVar scope = self scope 
		and: [aTempVar name = self name
		and: [aTempVar usage = self usage]]].

	
]

{ #category : #converting }
OCTempVariable >> asString [

	^ self name
]

{ #category : #queries }
OCTempVariable >> astNodes [
	^self method variableNodes select: [ :each | each binding == self]
]

{ #category : #accessing }
OCTempVariable >> definingScope [
	^ scope
]

{ #category : #emitting }
OCTempVariable >> emitStore: methodBuilder [

	methodBuilder storeTemp: name. 
]

{ #category : #emitting }
OCTempVariable >> emitValue: methodBuilder [

	methodBuilder pushTemp: name.
]

{ #category : #escaping }
OCTempVariable >> escaping [
	^escaping
]

{ #category : #escaping }
OCTempVariable >> escaping: anObject [
	escaping := anObject
]

{ #category : #comparing }
OCTempVariable >> hash [

	^ name hash bitXor: (usage hash bitXor: scope hash).
]

{ #category : #accessing }
OCTempVariable >> index [
	^ index ifNil: [ 
		"if the index is nil, we are in an AST after name analysis but before
		IR generation. To fill the the index, we generate the ir."
		scope node methodNode ir. 
		index ifNil: [ self error: 'no temp index, should never happen' ] ]
]

{ #category : #accessing }
OCTempVariable >> index: anObject [
	index := anObject
]

{ #category : #initialization }
OCTempVariable >> initialize [
	super initialize.
	escaping := false.
]

{ #category : #testing }
OCTempVariable >> isCopying [
	^false
]

{ #category : #escaping }
OCTempVariable >> isEscaping [
	^escaping = #escapingRead or: [escaping = #escapingWrite]
]

{ #category : #escaping }
OCTempVariable >> isEscapingRead [
	^escaping = #escapingRead

]

{ #category : #escaping }
OCTempVariable >> isEscapingWrite [
	^escaping = #escapingWrite

]

{ #category : #testing }
OCTempVariable >> isLocal [

	^true
]

{ #category : #'read/write usage' }
OCTempVariable >> isRead [
	^usage = #read

]

{ #category : #testing }
OCTempVariable >> isReferenced [
	^self isUnused not
]

{ #category : #testing }
OCTempVariable >> isRemote [
	^false
]

{ #category : #'read/write usage' }
OCTempVariable >> isRepeatedWrite [
	^usage = #repeatedWrite

]

{ #category : #testing }
OCTempVariable >> isStoringTempVector [
	"I am a temp that stores a temp vector. Those generated temps have a invalid name starting with 0"
	^name first = $0.
]

{ #category : #testing }
OCTempVariable >> isTemp [

	^ true
]

{ #category : #testing }
OCTempVariable >> isTempVectorTemp [
	^false
]

{ #category : #'read/write usage' }
OCTempVariable >> isUninitialized [

	^ self isWrite not
]

{ #category : #'read/write usage' }
OCTempVariable >> isUnused [
	"when the var is never read or written, it is not used.
	Note: we have a special #arg use which means arguments are never unused"
	^ usage isNil
]

{ #category : #'read/write usage' }
OCTempVariable >> isWrite [
	^ usage = #write or: [ self isRepeatedWrite ]
]

{ #category : #escaping }
OCTempVariable >> markEscapingRead [
	escaping = #escapingWrite ifFalse: [escaping := #escapingRead]
]

{ #category : #escaping }
OCTempVariable >> markEscapingWrite [
	escaping := #escapingWrite.
	self isRepeatedWrite ifFalse:[usage := #write]
]

{ #category : #'read/write usage' }
OCTempVariable >> markRead [
	"reading does not change a #write, nor an #arg"
	usage ifNil: [usage := #read]
]

{ #category : #escaping }
OCTempVariable >> markRepeatedWrite [
	"same as write"
	self markWrite.
	usage := #repeatedWrite
]

{ #category : #escaping }
OCTempVariable >> markWrite [

	"if an escaping var is wrote to later, it needs to be remote"
	self isEscaping 
		ifTrue: [self markEscapingWrite].
	"write is the strongest use: a read is turned into a write"
	usage := #write.
]

{ #category : #queries }
OCTempVariable >> method [
	^scope node methodNode methodClass>>scope node methodNode selector
]

{ #category : #accessing }
OCTempVariable >> originalVar [
	^ self
]

{ #category : #printing }
OCTempVariable >> printOn: stream [

	stream nextPutAll: self name
]

{ #category : #debugging }
OCTempVariable >> readFromContext: aContext scope: contextScope [
	| definitionContext |
	definitionContext := contextScope lookupDefiningContextForVariable: self startingFrom: aContext.
	^ self readFromLocalContext: definitionContext
]

{ #category : #debugging }
OCTempVariable >> readFromLocalContext: aContext [

	^ aContext tempAt: self index
]

{ #category : #debugging }
OCTempVariable >> readInContext: aContext [

	| contextScope |
	contextScope := aContext astScope.
	^self readFromContext: aContext scope: contextScope
]

{ #category : #accessing }
OCTempVariable >> scope [

	^ scope
]

{ #category : #initializing }
OCTempVariable >> scope: aLexicalScope [

	scope := aLexicalScope
]

{ #category : #accessing }
OCTempVariable >> usage [

	^ usage
]

{ #category : #accessing }
OCTempVariable >> usage: anObject [

	usage := anObject
]

{ #category : #queries }
OCTempVariable >> usingMethods [
	self isUnused ifTrue: [ ^#() ].
	^{self method}
]

{ #category : #debugging }
OCTempVariable >> write: aValue inContext: aContext [

	| contextScope |
	contextScope := aContext astScope.
	self writeFromContext: aContext scope: contextScope value: aValue
]

{ #category : #debugging }
OCTempVariable >> writeFromContext: aContext scope: contextScope value: aValue [

	| definitionContext |
	definitionContext := contextScope lookupDefiningContextForVariable: self startingFrom: aContext.
	^self writeFromLocalContext: definitionContext put: aValue
]

{ #category : #debugging }
OCTempVariable >> writeFromLocalContext: aContext put: aValue [

	^ aContext tempAt: self index put: aValue
]
