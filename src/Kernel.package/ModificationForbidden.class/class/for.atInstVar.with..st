instance creation
for: anObject atInstVar: fieldIndex with: newValue

	^self new 
		object: anObject;
		fieldIndex: fieldIndex;
		newValue: newValue;
		retrySelector: #instVarAt:put: