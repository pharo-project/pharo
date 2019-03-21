Class {
	#name : #ShCreateClassTest,
	#superclass : #ShAbstractClassBuilderTest,
	#category : #'Shift-ClassBuilder-Tests'
}

{ #category : #tests }
ShCreateClassTest >> testClassVariables [
	builder name: #SHClassWithClassVariable.
	result := builder
		classVariables: {(#aClassVariable => ClassVariable)};
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateClassVariables: #(aClassVariable)
]

{ #category : #tests }
ShCreateClassTest >> testClassVariablesAsArray [
	builder name: #SHClassWithClassVariable.
	result := builder
		classVariablesFromString: #(aClassVariable);
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateClassVariables: #(aClassVariable)
]

{ #category : #tests }
ShCreateClassTest >> testClassVariablesAsString [
	builder name: #SHClassWithClassVariable.
	result := builder
		classVariablesFromString: 'aClassVariable';
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateClassVariables: #(aClassVariable)
]

{ #category : #tests }
ShCreateClassTest >> testClassVariablesWithSlot [
	builder name: #SHClassWithClassVariable.
	self should: [ builder classVariablesFromString: {#aClassVariable asSlot} ] raise: Error.
]

{ #category : #tests }
ShCreateClassTest >> testClassWithClassSlots [
	builder
		name: #SHTestClassWithClassSlots;
		slots: #(anSlot);
		classSlots: #(aClassSlot).

	result := builder build.

	self validateResult.
	self validateSuperclass: Object.
	self
		validateSlots:
			{#anSlot => InstanceVariableSlot}.
			
	self
		validateClassSlots:
			{#aClassSlot => InstanceVariableSlot.}
		superclass: Object
]

{ #category : #tests }
ShCreateClassTest >> testClassWithInheritedSlots [
	builder
		name: #SHTestClassWithInheritedSlots;
		superclassName: #ShCBClassWithInstanceVariables;
		slots: #(anSlot anotherSlot).

	result := builder build.

	self validateResult.
	self validateSuperclass: ShCBClassWithInstanceVariables.
	self
		validateSlots:
			{#inheritedSlot => InstanceVariableSlot.
			#anSlot => InstanceVariableSlot.
			#anotherSlot => InstanceVariableSlot}
]

{ #category : #tests }
ShCreateClassTest >> testClassWithSlots [
	builder
		name: #SHTestClassWithSlots;
		slots: #(anSlot anotherSlot).

	result := builder build.

	self validateResult.
	self validateSuperclass: Object.
	self
		validateSlots:
			{(#anSlot => InstanceVariableSlot).
			(#anotherSlot => InstanceVariableSlot)}
]

{ #category : #tests }
ShCreateClassTest >> testEmptyClass [
	builder name: #SHTestEmptyClass.
	result := builder build.
	
	self validateResult.
	self validateSuperclass: Object.

]

{ #category : #tests }
ShCreateClassTest >> testSharedPool [
	builder name: #SHClassWithSharedPool.
	result := builder
		sharedPools: 'ShTestSharedPool';
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateSharedPools: {ShTestSharedPool}
]

{ #category : #tests }
ShCreateClassTest >> testVariableLayout [
	builder name: #SHClassWithVariableLayout.
	result := builder
		slots: #(anSlot);
		layoutClass: VariableLayout ;
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateSlots: {#anSlot => InstanceVariableSlot}.

	self assert: result classLayout class equals: VariableLayout
]

{ #category : #tests }
ShCreateClassTest >> testWordLayout [
	builder name: #SHClassWithWordLayout.
	result := builder
		layoutClass: WordLayout;
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateSlots: #().

	self assert: result classLayout class equals: WordLayout
]
