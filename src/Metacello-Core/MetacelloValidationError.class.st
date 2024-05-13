"
MetacelloValidationError indicates that errors are to be expected if an attempt to use the configuration/version is made
"
Class {
	#name : 'MetacelloValidationError',
	#superclass : 'Object',
	#instVars : [
		'reasonCode',
		'callSite',
		'explanation'
	],
	#category : 'Metacello-Core-Validation',
	#package : 'Metacello-Core',
	#tag : 'Validation'
}

{ #category : 'instance creation' }
MetacelloValidationError class >> reasonCode: aSymbol callSite: aCallSite explanation: aString [

	^ self new
		  reasonCode: aSymbol;
		  callSite: aCallSite;
		  explanation: aString;
		  yourself
]

{ #category : 'accessing' }
MetacelloValidationError >> callSite [
	^ callSite
]

{ #category : 'accessing' }
MetacelloValidationError >> callSite: anObject [
	callSite := anObject
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
	self reasonCode ~~ #none ifTrue: [
		aStream
			space;
			nextPut: ${;
			space;
			print: self reasonCode;
			space;
			nextPut: $} ].

	(self configurationClass isNil and: [ self callSite isNil ]) ifTrue: [ ^ self ].

	aStream
		space;
		nextPut: $[;
		space.
	self configurationClass ifNotNil: [ :class |
		aStream
			print: class name;
			space ].
	self callSite ifNotNil: [ :site |
		aStream
			print: site name;
			space ].
	aStream nextPut: $]
]

{ #category : 'accessing' }
MetacelloValidationError >> reasonCode [

	^ reasonCode ifNil: [ reasonCode := #none ]
]

{ #category : 'accessing' }
MetacelloValidationError >> reasonCode: anObject [
	reasonCode := anObject
]
