Class {
	#name : 'ReBlockCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'errorMessage',
		'block'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReBlockCondition >> block: aBlock [

	block := aBlock
]

{ #category : 'checking' }
ReBlockCondition >> check [ 

	^ block value

]

{ #category : 'displaying' }
ReBlockCondition >> violationMessageOn: aStream [

	aStream nextPutAll: errorMessage
]

{ #category : 'checking' }
ReBlockCondition >> violatorErrorString: aString [

	errorMessage := aString
]
