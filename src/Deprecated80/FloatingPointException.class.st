"
I am FloatingPointException, an ArithmeticError indicating a problem in a floating point calculation.

Precision underflow or overflow could be considered FloatingPointExceptions.

ZeroDivide, DomainError and NaNException are examples of more specific ArithmeticErrors that can be signaled before an operation is actually attempted.
"
Class {
	#name : #FloatingPointException,
	#superclass : #ArithmeticError,
	#category : #Deprecated80
}

{ #category : #deprecation }
FloatingPointException class >> isDeprecated [

	^ true
]
