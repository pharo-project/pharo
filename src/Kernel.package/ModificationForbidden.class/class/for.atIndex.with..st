instance creation
for: anObject atIndex: fieldIndex with: newValue

	^self new 
		object: anObject;
		fieldIndex: fieldIndex;
		newValue: newValue;
		retrySelector: #at:put: