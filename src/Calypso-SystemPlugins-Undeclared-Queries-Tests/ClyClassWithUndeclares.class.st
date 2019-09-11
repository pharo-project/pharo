"
I am example class with methods which use undeclated variables
"
Class {
	#name : #ClyClassWithUndeclares,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-Undeclared-Queries-Tests'
}

{ #category : #'without undeclares' }
ClyClassWithUndeclares class >> withFloatReturn [
	^10.2
]

{ #category : #'with undeclares' }
ClyClassWithUndeclares >> method1WithUndeclares [
	^undeclaredStubInstVar1 printString
]

{ #category : #'with undeclares' }
ClyClassWithUndeclares >> method2WithUndeclares [
	undeclaredStubInstVar2 := 100
]
