"
A class that mimic CompiledMethod instances.
Answers to `run:with:in:` for the runtine behavior
"
Class {
	#name : #TestSpecialMethodObject,
	#superclass : #Object,
	#instVars : [
		'selector',
		'realMethod'
	],
	#category : #'Kernel-Tests-Special'
}

{ #category : #'reflective operations' }
TestSpecialMethodObject >> doesNotUnderstand: aMessage [

	^ realMethod perform: aMessage selector withArguments: aMessage arguments
]

{ #category : #accessing }
TestSpecialMethodObject >> realMethod [

	^ realMethod
]

{ #category : #accessing }
TestSpecialMethodObject >> realMethod: anObject [

	realMethod := anObject
]

{ #category : #evaluation }
TestSpecialMethodObject >> run: aSelector with: anArray in: aReceiver [

	^ #run:with:in: -> {
		  aSelector.
		  anArray.
		  aReceiver }
]

{ #category : #accessing }
TestSpecialMethodObject >> selector [

	^ selector
]

{ #category : #accessing }
TestSpecialMethodObject >> selector: anObject [

	selector := anObject
]
