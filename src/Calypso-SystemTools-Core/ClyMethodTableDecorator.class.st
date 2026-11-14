"
I am a base class for method table decorators.

I am annotated by decoration strategy in context of selected method.
So my subclasses should just implement decoration logic
"
Class {
	#name : 'ClyMethodTableDecorator',
	#superclass : 'ClyTableDecorator',
	#category : 'Calypso-SystemTools-Core-TableDecorators',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'TableDecorators'
}

{ #category : 'testing' }
ClyMethodTableDecorator class >> isAbstract [
	^self = ClyMethodTableDecorator
]
