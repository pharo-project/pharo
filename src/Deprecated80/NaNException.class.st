"
I am NaNException, an ArithmeticException signaled when Float nan was encountered where it was not allowed. 
"
Class {
	#name : #NaNException,
	#superclass : #ArithmeticError,
	#category : #Deprecated80
}

{ #category : #deprecation }
NaNException class >> isDeprecated [

	^ true
]
