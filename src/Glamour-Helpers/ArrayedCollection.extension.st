Extension { #name : #ArrayedCollection }

{ #category : #'*Glamour-Helpers' }
ArrayedCollection class >> with: anObject withAll: aCollection [

	| newArray |
	newArray := self new: aCollection size + 1.
	newArray at: 1 put: anObject.
	newArray
		replaceFrom: 2
		to: newArray size
		with: aCollection.
	^newArray
	
"Array with: 1 withAll: #(2 3 4)"
]
