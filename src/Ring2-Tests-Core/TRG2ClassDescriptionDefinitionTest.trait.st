Trait {
	#name : #TRG2ClassDescriptionDefinitionTest,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
TRG2ClassDescriptionDefinitionTest >> testCopyForBehaviorDefinitionForClassDescription [
	
	| env aClass copy |
	
	env := RG2Environment new.
	aClass := env ensureClassNamed: #SomeClass.
	aClass layout: (RG2ByteLayoutDefinition parent: aClass).
		
	copy := aClass copyForBehaviorDefinition.
	
	self deny: copy == aClass. 
	self deny: copy layout == aClass layout. 
	self assert: copy layout isByteLayout.
	self assert: (copy layout parent == copy).
]

{ #category : #tests }
TRG2ClassDescriptionDefinitionTest >> testTypes [

	| newBehavior newSuperclass |
	newBehavior := self behaviorClass unnamed.
	
	self deny: newBehavior isBytes.
	self deny: newBehavior isCompiledMethod.
	self deny: newBehavior isEphemeron.
	self deny: newBehavior isWeak.
	self deny: newBehavior isWords.

	
]
