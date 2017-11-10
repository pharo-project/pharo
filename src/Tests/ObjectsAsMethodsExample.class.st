Class {
	#name : #ObjectsAsMethodsExample,
	#superclass : #AbstractObjectsAsMethod,
	#category : #Tests-ObjectsAsMethods
}

{ #category : #'as yet unclassified' }
ObjectsAsMethodsExample >> add: a with: b [
	^a + b
]

{ #category : #'as yet unclassified' }
ObjectsAsMethodsExample >> answer42 [
	^42
]

{ #category : #'as yet unclassified' }
ObjectsAsMethodsExample >> run: oldSelector with: arguments in: aReceiver [
	^self perform: oldSelector withArguments: arguments
]
