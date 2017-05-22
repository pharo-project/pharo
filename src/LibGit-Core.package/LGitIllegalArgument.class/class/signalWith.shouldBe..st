signalling
signalWith: anObject shouldBe: aClass
	^ self new
		object: anObject;
		expectedClass: aClass;
		signal;
		yourself