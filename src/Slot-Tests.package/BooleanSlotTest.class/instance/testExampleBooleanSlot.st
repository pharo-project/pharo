tests
testExampleBooleanSlot

	| object slot reader writer |
	
	slot := #slot1 => BooleanSlot.

	aClass := self make: [ :builder |
		builder 
			slots: {slot}
		].

	self assert: (slot instVarNamed: 'baseSlot') isNotNil.

	object := aClass new. 
	"test refletive write and read"
	slot write: true to: object.
	self assert: (slot read: object) = true.
	
		"compiled accessors to test code gen"
	reader := String streamContents: [ :str |
		str 
			nextPutAll: slot name;
			cr;tab;
			nextPutAll: ' ^';
			nextPutAll: slot name.
		 ].
	writer := String streamContents: [ :str |
		str 
			nextPutAll: slot name;
			nextPutAll: ': anObject';
			cr;tab;
			nextPutAll: slot name;
			nextPutAll: ':= anObject.'.
		].
	aClass compile: reader classified: 'accessing'.
	aClass compile: writer classified: 'accessing'.
	
	self assert: object slot1 == true.
	object slot1: false.
	self assert: object slot1 == false.