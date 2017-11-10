Class {
	#name : #MetacelloValidationIssue,
	#superclass : #Object,
	#instVars : [
		'configurationClass',
		'explanation',
		'reasonCode',
		'callSite'
	],
	#category : #Metacello-Core-Validation
}

{ #category : #'instance creation' }
MetacelloValidationIssue class >> configurationClass: aClass reasonCode: aSymbol callSite: aCallSite explanation: aString [

	^(self new)
		configurationClass: aClass;
		reasonCode: aSymbol;
		callSite: aCallSite;
		explanation: aString;
		yourself
]

{ #category : #accessing }
MetacelloValidationIssue >> callSite [
	^ callSite
]

{ #category : #accessing }
MetacelloValidationIssue >> callSite: anObject [
	callSite := anObject
]

{ #category : #accessing }
MetacelloValidationIssue >> configurationClass [
	^ configurationClass
]

{ #category : #accessing }
MetacelloValidationIssue >> configurationClass: aClass [
	configurationClass := aClass
]

{ #category : #accessing }
MetacelloValidationIssue >> explanation [
	^ explanation
]

{ #category : #accessing }
MetacelloValidationIssue >> explanation: aString [
	explanation := aString
]

{ #category : #testing }
MetacelloValidationIssue >> isCritical [

	^self isError or: [ self isCriticalWarning ]
]

{ #category : #testing }
MetacelloValidationIssue >> isCriticalWarning [

	^false
]

{ #category : #testing }
MetacelloValidationIssue >> isError [

	^false
]

{ #category : #testing }
MetacelloValidationIssue >> isWarning [

	^false
]

{ #category : #accessing }
MetacelloValidationIssue >> label [

	^''
]

{ #category : #printing }
MetacelloValidationIssue >> printOn: aStream [
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
				nextPutAll: self reasonCode asString;
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
						nextPutAll: self configurationClass name asString;
						space ].
			self callSite ~~ nil
				ifTrue: [ 
					aStream
						nextPutAll: self callSite name asString;
						space ].
			aStream nextPut: $] ]
]

{ #category : #accessing }
MetacelloValidationIssue >> reasonCode [

	reasonCode == nil ifTrue: [ reasonCode := #none ].
	^ reasonCode
]

{ #category : #accessing }
MetacelloValidationIssue >> reasonCode: anObject [
	reasonCode := anObject
]
