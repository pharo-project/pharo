instance creation
on: anEvent send: aSelector to: anObject
	^self new
		event: anEvent;
		selector: aSelector;
		recipient: anObject;
		yourself