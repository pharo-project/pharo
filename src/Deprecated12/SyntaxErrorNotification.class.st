"
The role of SyntaxErrorNotification is to interface witht the compiler error handling of the system, see #syntaxErrorNotificationDefaultAction:

This is not a subclass of Error as the exception can lead to the user editing the code and continue compilation

TODO: unify with the OCSemanticWarning hierarchy
"
Class {
	#name : #SyntaxErrorNotification,
	#superclass : #CodeError,
	#category : #Deprecated12
}

{ #category : #exceptionInstantiator }
SyntaxErrorNotification class >> inClass: aClass withCode: codeString doitFlag: doitFlag errorMessage: errorString location: location [
	self new
		sourceCode: codeString;
		messageText: errorString;
		location: location;
		signal
]
