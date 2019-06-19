"
I am a base class for method table decorators.

I am annotated by decoration strategy in context of selected method.
So my subclasses should just implement decoration logic
"
Class {
	#name : #ClyMethodTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyMethodTableDecorator class >> decorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyMethod asCalypsoItemContext
]

{ #category : #testing }
ClyMethodTableDecorator class >> isAbstract [
	^self = ClyMethodTableDecorator  
]
