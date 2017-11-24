"
I am an acceptor in the case a class definition has been entered.
My action is then to compile the definition as a class object
"
Class {
	#name : #ClassDefinitionAcceptor,
	#superclass : #AbstractAcceptor,
	#category : #'Nautilus-Acceptors'
}

{ #category : #protocol }
ClassDefinitionAcceptor >> accept: aText notifying: aController [

	(self isClassDefinitionExpression: aText)
		ifFalse: [ ^ self notifyNoClassDefintion ].
		
	self model compileAClassFrom: aText notifying: aController
]

{ #category : #private }
ClassDefinitionAcceptor >> isClassDefinitionExpression: aSource [
	[ RBParser parseExpression: aSource ]
		on: Notification
		do: [ ^ false ].
	^ true
]

{ #category : #private }
ClassDefinitionAcceptor >> notifyNoClassDefintion [
	"if this was not a class definition, the user has tried to
	define a method without selecting a class"

	^ UIManager default alert: 'No class selected'
]
