instance creation
on: anObject indentBy: aNumber
	^ self new
		setElement: anObject;
		setIndent: aNumber;
		yourself