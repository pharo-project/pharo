Class {
	#name : #ObjectsAsMethodsExample,
	#superclass : #AbstractObjectsAsMethod,
	#category : #'Tests-ObjectsAsMethods'
}

{ #category : #example }
ObjectsAsMethodsExample >> add: a with: b [
	^a + b
]

{ #category : #example }
ObjectsAsMethodsExample >> answer42 [
	^42
]

{ #category : #evaluation }
ObjectsAsMethodsExample >> run: oldSelector with: arguments in: aReceiver [
	^self perform: oldSelector withArguments: arguments
]
