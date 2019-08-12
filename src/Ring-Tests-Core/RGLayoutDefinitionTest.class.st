Class {
	#name : #RGLayoutDefinitionTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGLayoutDefinitionTest >> testImplicitEnvironment [
	
	RGLayout allSubclasses do: [ :layoutClass |
		| layout | 
		layout  := layoutClass unnamed.
		self checkImplicitSingleClassEnvironmentOf: layout parent. ].	


]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsBitsLayout [

	self assert: (RGByteLayout unnamed isBitsLayout).
	self assert: (RGWordLayout unnamed isBitsLayout).
	self assert: (RGCompiledMethodLayout unnamed isBitsLayout).
	self deny: (RGFixedLayout unnamed isBitsLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsBytesLayout [

	self assert: (RGByteLayout unnamed isByteLayout).
	self assert: (RGCompiledMethodLayout unnamed isByteLayout).
	self deny: (RGFixedLayout unnamed isByteLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsCompiledMethodLayout [

	self assert: (RGCompiledMethodLayout unnamed isCompiledMethodLayout).
	self deny: (RGFixedLayout unnamed isCompiledMethodLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsEmptyLayout [

	| layout layoutClass | 
		
	layoutClass := 	RGEmptyLayout.
	layout  := layoutClass unnamed.
	self assert: (layout isEmptyLayout).
	(RGLayout allSubclasses copyWithout: layoutClass) do: [ :each |
		self deny: (each unnamed isEmptyLayout) ]
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsEphemeronLayout [

	self assert: (RGEphemeronLayout unnamed isEphemeronLayout).
	self deny: (RGFixedLayout unnamed isEphemeronLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsFixedLayout [

	self assert: (RGFixedLayout unnamed isFixedLayout).
	self deny: (RGWordLayout unnamed isFixedLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsImmediateLayout [

	self assert: (RGImmediateLayout unnamed isImmediateLayout).
	self deny: (RGFixedLayout unnamed isImmediateLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsObjectLayout [

	self assert: (RGImmediateLayout unnamed isObjectLayout).
	self deny: (RGEmptyLayout unnamed isObjectLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsPointerLayout [

	self assert: (RGFixedLayout unnamed isPointerLayout).
	self deny: (RGEmptyLayout unnamed isPointerLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsVariableLayout [

	self assert: (RGVariableLayout unnamed isVariableLayout).
	self assert: (RGBitsLayout unnamed isVariableLayout).
	self deny: (RGFixedLayout unnamed isVariableLayout).
	self assert: (RGCompiledMethodLayout unnamed isVariableLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsWeakLayout [

	self assert: (RGWeakLayout unnamed isWeakLayout).
	self deny: (RGEmptyLayout unnamed isWeakLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testIsWordLayouts [

	self assert: (RGWordLayout unnamed isWordLayout).
	self deny: (RGEmptyLayout unnamed isWordLayout).
	
]

{ #category : #tests }
RGLayoutDefinitionTest >> testNewLayouts [

	RGLayout allSubclasses do: [ :layoutClass |
		| layout | 
		layout  := layoutClass unnamed.
		self assert: (layout isRingResolved).
		self assert: (layout hasUnresolved: #name).
		self assert: (layout isLayout).
		self assert: (layout allSlots isEmpty).
		self assert: (layout instVarNames isEmpty). ]
]

{ #category : #tests }
RGLayoutDefinitionTest >> testResolvingConsistency [

	| anObject |
	anObject := RGPointerLayout unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]
