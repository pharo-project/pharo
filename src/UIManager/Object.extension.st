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

	self deprecated: 'This method will be removed of Object because the API should not hold UI interactions.
	The most simple way to replace the usage of #inform: is to use `InformativeNotification signal: aString`. 
	But if you have access to a spec application or presenter you can use the #inform: implemented on those instead.'. "DO NOT CALL THIS METHOD!"
	aString isEmptyOrNil ifFalse: [ UIManager default inform: aString ]
]

{ #category : '*UIManager' }
Object >> uiManager [

	self halt.
	^ UIManager default
]
