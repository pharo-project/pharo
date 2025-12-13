"
MetacelloValidationError indicates that errors are to be expected if an attempt to use the configuration/version is made
"
Class {
	#name : 'MetacelloValidationError',
	#superclass : 'Object',
	#instVars : [
		'explanation'
	],
	#category : 'Metacello-Core-Validation',
	#package : 'Metacello-Core',
	#tag : 'Validation'
}

{ #category : 'instance creation' }
MetacelloValidationError class >> explanation: aString [

	^ self new
		  explanation: aString;
		  yourself
]

{ #category : 'accessing' }
MetacelloValidationError >> explanation [
	^ explanation
]

{ #category : 'accessing' }
MetacelloValidationError >> explanation: aString [
	explanation := aString
]

{ #category : 'accessing' }
MetacelloValidationError >> label [

	^'Error'
]

{ #category : 'printing' }
MetacelloValidationError >> printOn: aStream [

	aStream
		nextPutAll: self label;
		nextPut: $:;
		space;
		nextPutAll: self explanation.

	aStream
		space;
		nextPutAll: '{ incomplete project spec }'.

	self flag: #ConfigurationRemoval. "Not related to configuration removal but this code is broken and can be repaired easily"
	self configurationClass isNil ifTrue: [ ^ self ].

	aStream
		space;
		nextPut: $[;
		space.
	self configurationClass ifNotNil: [ :class |
			aStream
				print: class name;
				space ].
	aStream nextPut: $]
]
