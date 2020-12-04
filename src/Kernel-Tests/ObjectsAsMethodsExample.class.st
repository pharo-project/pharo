"
Example for an object that is installed as a compiled method. The VM calls run:with:in:
"
Class {
	#name : #ObjectsAsMethodsExample,
	#superclass : #AbstractObjectsAsMethod,
	#category : #'Kernel-Tests-Methods'
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
