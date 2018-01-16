Class {
	#name : #RG2LayoutDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2LayoutDefinitionTest >> testImplicitEnvironment [
	
	RG2LayoutDefinition allSubclasses do: [ :layoutClass |
		| layout | 
		layout  := layoutClass unnamed.
		self checkImplicitSingleClassEnvironmentOf: layout parent. ].	


]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsBitsLayout [

	self assert: (RG2ByteLayoutDefinition unnamed isBitsLayout).
	self assert: (RG2WordLayoutDefinition unnamed isBitsLayout).
	self assert: (RG2CompiledMethodLayoutDefinition unnamed isBitsLayout).
	self deny: (RG2FixedLayoutDefinition unnamed isBitsLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsBytesLayout [

	self assert: (RG2ByteLayoutDefinition unnamed isByteLayout).
	self assert: (RG2CompiledMethodLayoutDefinition unnamed isByteLayout).
	self deny: (RG2FixedLayoutDefinition unnamed isByteLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsCompiledMethodLayout [

	self assert: (RG2CompiledMethodLayoutDefinition unnamed isCompiledMethodLayout).
	self deny: (RG2FixedLayoutDefinition unnamed isCompiledMethodLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsEmptyLayout [

	| layout layoutClass | 
		
	layoutClass := 	RG2EmptyLayoutDefinition.
	layout  := layoutClass unnamed.
	self assert: (layout isEmptyLayout).
	(RG2LayoutDefinition allSubclasses copyWithout: layoutClass) do: [ :each |
		self deny: (each unnamed isEmptyLayout) ]
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsEphemeronLayout [

	self assert: (RG2EphemeronLayoutDefinition unnamed isEphemeronLayout).
	self deny: (RG2FixedLayoutDefinition unnamed isEphemeronLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsFixedLayout [

	self assert: (RG2FixedLayoutDefinition unnamed isFixedLayout).
	self deny: (RG2WordLayoutDefinition unnamed isFixedLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsImmediateLayout [

	self assert: (RG2ImmediateLayoutDefinition unnamed isImmediateLayout).
	self deny: (RG2FixedLayoutDefinition unnamed isImmediateLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsObjectLayout [

	self assert: (RG2ImmediateLayoutDefinition unnamed isObjectLayout).
	self deny: (RG2EmptyLayoutDefinition unnamed isObjectLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsPointerLayout [

	self assert: (RG2FixedLayoutDefinition unnamed isPointerLayout).
	self deny: (RG2EmptyLayoutDefinition unnamed isPointerLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsVariableLayout [

	self assert: (RG2VariableLayoutDefinition unnamed isVariableLayout).
	self assert: (RG2BitsLayoutDefinition unnamed isVariableLayout).
	self deny: (RG2FixedLayoutDefinition unnamed isVariableLayout).
	self assert: (RG2CompiledMethodLayoutDefinition unnamed isVariableLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsWeakLayout [

	self assert: (RG2WeakLayoutDefinition unnamed isWeakLayout).
	self deny: (RG2EmptyLayoutDefinition unnamed isWeakLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testIsWordLayouts [

	self assert: (RG2WordLayoutDefinition unnamed isWordLayout).
	self deny: (RG2EmptyLayoutDefinition unnamed isWordLayout).
	
]

{ #category : #tests }
RG2LayoutDefinitionTest >> testNewLayouts [

	RG2LayoutDefinition allSubclasses do: [ :layoutClass |
		| layout | 
		layout  := layoutClass unnamed.
		self assert: (layout isRing2Resolved).
		self assert: (layout hasUnresolved: #name).
		self assert: (layout isLayout).
		self assert: (layout allSlots isEmpty).
		self assert: (layout instVarNames isEmpty). ]
]
