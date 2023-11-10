"
MetacelloValidationError indicates that errors are to be expected if an attempt to use the configuration/version is made
"
Class {
	#name : 'MetacelloValidationError',
	#superclass : 'Object',
	#instVars : [
		'reasonCode',
		'callSite',
		'configurationClass',
		'explanation'
	],
	#category : 'Metacello-Core-Validation',
	#package : 'Metacello-Core',
	#tag : 'Validation'
}

{ #category : 'instance creation' }
MetacelloValidationError class >> configurationClass: aClass reasonCode: aSymbol callSite: aCallSite explanation: aString [

	^(self new)
		configurationClass: aClass;
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
MetacelloValidationError >> configurationClass [
	^ configurationClass
]

{ #category : 'accessing' }
MetacelloValidationError >> configurationClass: aClass [
	configurationClass := aClass
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
	self reasonCode ~~ #none
		ifTrue: [ 
			aStream
				space;
				nextPut: ${;
				space;
				print: self reasonCode;
				space;
				nextPut: $} ].
	(self configurationClass ~~ nil or: [ self callSite ~~ nil ])
		ifTrue: [ 
			aStream
				space;
				nextPut: $[;
				space.
			self configurationClass ~~ nil
				ifTrue: [ 
					aStream
						print: self configurationClass name;
						space ].
			self callSite ~~ nil
				ifTrue: [ 
					aStream
						print: self callSite name;
						space ].
			aStream nextPut: $] ]
]

{ #category : 'accessing' }
MetacelloValidationError >> reasonCode [

	^ reasonCode ifNil: [ reasonCode := #none ]
]

{ #category : 'accessing' }
MetacelloValidationError >> reasonCode: anObject [
	reasonCode := anObject
]
