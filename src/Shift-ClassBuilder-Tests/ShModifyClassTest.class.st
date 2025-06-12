Class {
	#name : 'ShModifyClassTest',
	#superclass : 'ShAbstractClassBuilderTest',
	#category : 'Shift-ClassBuilder-Tests-Base',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
ShModifyClassTest >> testChangingSuperclassOfAClassWithDuplicatedSlotsIsNotApplied [

	builder name: #ShCBClassWithSubclass.
	builder superclassName: #ShCBAClasWithAnSlot.

	self should: [result := builder build] raise: DuplicatedSlotName.

	result := self class environment classNamed: #ShCBClassWithSubclass.

	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #(  ).

]

{ #category : 'tests' }
ShModifyClassTest >> testChangingSuperclassOfAClassWithDuplicatedSlotsIsNotAppliedInDeeperHierarchy [

	builder name: #ShCBClassWithSubclass.
	builder superclassName: #ShCBAClasWithAnotherSlot.

	self should: [result := builder build] raise: DuplicatedSlotName.

	result := self class environment classNamed: #ShCBClassWithSubclass.

	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #(  ).

]

{ #category : 'tests' }
ShModifyClassTest >> testEmptyClass [

	builder name: #ShCBEmptyClass.
	result := builder build.

	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #(  ).
	self assert: result hasComment
]

{ #category : 'tests' }
ShModifyClassTest >> testEmptyClassWithMethods [

	builder name: #ShCBEmptyClassWithMethods.
	result := builder build.

	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #( aMethod anotherMethod: )
]
