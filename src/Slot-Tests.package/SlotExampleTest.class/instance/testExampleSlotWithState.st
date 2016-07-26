tests
testExampleSlotWithState
	| slot reader writer |
	
	aClass := self make: [ :builder |
		builder 
			slots: {#slot1 =>ExampleSlotWithState  }
		].

	self assert: (aClass hasSlotNamed: #slot1).
	
	"test refletive write and read"
	slot := aClass slotNamed: #slot1.
	slot write: 5 to: aClass new.
	self assert: (slot read: aClass new) = 5.
	
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
	
	aClass new slot1: 10.
	self assert: aClass new slot1 = 10.
	
	
	
	
	