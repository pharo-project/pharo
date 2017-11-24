"
I am ZeroDivide, an ArithmeticError that may be signaled when a mathematical division by 0 is attempted.
"
Class {
	#name : #ZeroDivide,
	#superclass : #ArithmeticError,
	#instVars : [
		'dividend'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #exceptioninstantiator }
ZeroDivide class >> dividend: argument [
	^self new dividend: argument; yourself
]

{ #category : #signaling }
ZeroDivide class >> signalWithDividend: aDividend [

	^(self dividend: aDividend) signal
]

{ #category : #exceptiondescription }
ZeroDivide >> dividend [
	"Answer the number that was being divided by zero."

	^dividend
]

{ #category : #exceptionbuilder }
ZeroDivide >> dividend: argument [
	"Specify the number that was being divided by zero."

	dividend := argument
]

{ #category : #exceptiondescription }
ZeroDivide >> isResumable [
	"Determine whether an exception is resumable."

	^true
]
