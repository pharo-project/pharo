"
A class with a Pharo doc comment to be used in tests
"
Class {
	#name : #ClassWithPharoDocComment,
	#superclass : #Object,
	#category : #'PharoDocComment-Tests-Resources'
}

{ #category : #resources }
ClassWithPharoDocComment >> methodWithCorrectExpression [
	"a first comment"
	
	"3 + 4 >>> 7"
	
	^ 42
]
