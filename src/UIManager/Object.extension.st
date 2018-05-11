Extension { #name : #Object }

{ #category : #'*UIManager' }
Object >> confirm: queryString [
	"Put up a yes/no menu with caption queryString. Answer true if the 
	response is yes, false if no. This is a modal question--the user must 
	respond yes or no."

	"nil confirm: 'Are you hungry?'"

	^ UIManager default confirm: queryString
]

{ #category : #'*UIManager' }
Object >> inform: aString [
	"Display a message for the user to read and then dismiss."

	aString isEmptyOrNil
		ifFalse: [ UIManager default inform: aString ]
]

{ #category : #'*UIManager' }
Object >> primitiveError: aString [ 
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	UIManager default onPrimitiveError: aString
]
