tests
testCreatingASimpleClass
	| exportedClass slots classSlots obj |
	
	exportedClass := HEClass for: HETeSimpleClass. 
	self changeNameOf: exportedClass to: #HETeSimpleClassNew.
	
	aClass := builder build: exportedClass.
	
	self assert: aClass name equals: #HETeSimpleClassNew.
	self assert: aClass class name equals: #'HETeSimpleClassNew class'.
	
	slots :=  {#anInstanceVariable => InstanceVariableSlot}.
	(slots at:1) index: 1.	
	self assert: aClass slots equals: slots.
	
	classSlots :=  {#aClassInstanceVariable => InstanceVariableSlot}.
	(classSlots at:1) index: (Object class allSlots size + 1).		
	self assert: aClass class slots equals: classSlots.
	
	self assert: aClass traitComposition isEmpty.
	self assert: aClass classVariables equals: { #AClassVariable => ClassVariable }.
	
	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self assert: aClass traitComposition isEmpty.
	self assert: aClass class traitComposition isEmpty.

	obj := aClass newInstance.
	
	obj anInstanceVariable: 25.
	self assert: obj anInstanceVariable equals: 25.
	self assert: obj otherMethod equals: 25.
	self assert: obj usingAConstant equals: 42.