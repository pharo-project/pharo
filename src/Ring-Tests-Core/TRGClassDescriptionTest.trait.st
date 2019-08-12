Trait {
	#name : #TRGClassDescriptionTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
TRGClassDescriptionTest >> testCopyForBehaviorDefinitionForClassDescription [
	
	| env aClass copy |
	
	env := RGEnvironment new.
	aClass := env ensureClassNamed: #SomeClass.
	aClass layout: (RGByteLayout parent: aClass).
		
	copy := aClass copyForBehaviorDefinition.
	
	self deny: copy == aClass. 
	self deny: copy layout == aClass layout. 
	self assert: copy layout isByteLayout.
	self assert: (copy layout parent == copy).
]

{ #category : #tests }
TRGClassDescriptionTest >> testTypes [

	| newBehavior |
	newBehavior := self behaviorClass unnamed.
	
	self deny: newBehavior isBytes.
	self deny: newBehavior isCompiledMethod.
	self deny: newBehavior isEphemeron.
	self deny: newBehavior isWeak.
	self deny: newBehavior isWords.

	
]
