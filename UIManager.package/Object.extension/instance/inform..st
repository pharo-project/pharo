*UIManager
inform: aString
	"Display a message for the user to read and then dismiss."

	aString isEmptyOrNil ifFalse: [UIManager default inform: aString]