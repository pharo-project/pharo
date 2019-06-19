"
I am a base class for package table decorators.

I am annotated by decoration strategy in context of selected package.
So my subclasses should just implement decoration logic
"
Class {
	#name : #ClyPackageTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyPackageTableDecorator class >> decorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: RPackage asCalypsoItemContext
]

{ #category : #testing }
ClyPackageTableDecorator class >> isAbstract [
	^self = ClyPackageTableDecorator 
]
