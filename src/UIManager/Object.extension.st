Extension { #name : 'Object' }

{ #category : '*UIManager' }
Object >> confirm: queryString [
	"Put up a yes/no menu with caption queryString. Answer true if the
	response is yes, false if no. This is a modal question--the user must
	respond yes or no."

	self halt.
	"DO NOT CALL THIS METHOD!"
	^ UIManager default confirm: queryString
]

{ #category : '*UIManager' }
Object >> inform: aString [
	"Display a message for the user to read and then dismiss."
	
	self halt.
	"DO NOT CALL THIS METHOD!"
	aString isEmptyOrNil
		ifFalse: [ UIManager default inform: aString ]
]

{ #category : '*UIManager' }
Object >> uiManager [

	self halt.
	^ UIManager default
]
