instance creation
on: anEvent send: aMessageSelector to: anObject withValue: aValue
	^self new
		event: anEvent;
		selector: aMessageSelector;
		recipient: anObject;
		valueParameter: aValue;
		yourself