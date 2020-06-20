"
I model temp variables. With Closures, there are two kinds: Copying and those that are stored in a so called temp vector, a heap allocated array that itself is stored in a copying temp variable.
"
Class {
	#name : #OCTempVariable,
	#superclass : #OCAbstractLocalVariable,
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

{ #category : #escaping }
OCTempVariable >> markEscapingRead [
	escaping = #escapingWrite ifFalse: [escaping := #escapingRead]
]

{ #category : #escaping }
OCTempVariable >> markEscapingWrite [
	escaping := #escapingWrite.
	self isRepeatedWrite ifFalse:[usage := #write]
]

{ #category : #escaping }
OCTempVariable >> markRepeatedWrite [
	"same as write"
	self markWrite.
	super markRepeatedWrite.
]

{ #category : #escaping }
OCTempVariable >> markWrite [

	"if an escaping var is wrote to later, it needs to be remote"
	self isEscaping 
		ifTrue: [self markEscapingWrite].
	super markWrite.
]

{ #category : #debugging }
OCTempVariable >> readFromContext: aContext [

	| contextScope |
	contextScope := aContext astScope.
	^self readFromContext: aContext scope: contextScope
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
OCTempVariable >> writeFromContext: aContext scope: contextScope value: aValue [

	| definitionContext |
	definitionContext := contextScope lookupDefiningContextForVariable: self startingFrom: aContext.
	^self writeFromLocalContext: definitionContext put: aValue
]

{ #category : #debugging }
OCTempVariable >> writeFromLocalContext: aContext put: aValue [

	^ aContext tempAt: self index put: aValue
]
