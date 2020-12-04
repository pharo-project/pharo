"
This exception is raised when accessing deprecated slots
"
Class {
	#name : #SlotDeprecation,
	#superclass : #Deprecation,
	#category : #'VariablesLibrary-DeprecatedSlot'
}

{ #category : #accessing }
SlotDeprecation >> explanationString [
	^explanationString ifNil: [ '' ]
]

{ #category : #accessing }
SlotDeprecation >> messageText [
	^ String
		streamContents: [ :str | 
			str
				nextPutAll: 'The instance variable ';
				nextPutAll: context receiver name;
				nextPutAll: ' accessed in ';
				nextPutAll: context sender method name;
				nextPutAll: ' has been deprecated. ';
				nextPutAll: self explanationString ]
]
