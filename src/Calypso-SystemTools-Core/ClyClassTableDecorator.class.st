"
I am a base class for class table decorators.

I am annotated by decoration strategy in context of selected class.
So my subclasses should just implement decoration logic
"
Class {
	#name : #ClyClassTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyClassTableDecorator class >> decorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyClass asCalypsoItemContext
]

{ #category : #testing }
ClyClassTableDecorator class >> isAbstract [
	^self = ClyClassTableDecorator 
]
