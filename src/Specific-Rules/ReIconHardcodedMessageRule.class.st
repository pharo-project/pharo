"
According to the new API you should use

	self iconNamed: #symbol
	
Instead of

	Smalltalk ui icons iconNamed: #symbol
"
Class {
	#name : 'ReIconHardcodedMessageRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'Specific-Rules-API Change',
	#package : 'Specific-Rules',
	#tag : 'API Change'
}

{ #category : 'accessing' }
ReIconHardcodedMessageRule class >> group [

	^ self apiChangeGroup
]

{ #category : 'accessing' }
ReIconHardcodedMessageRule class >> ruleName [
	^ 'Use self iconNamed: #symbol instead of asIcon or Smalltalk ui icons iconNamed: #symbol'
]

{ #category : 'accessing' }
ReIconHardcodedMessageRule class >> severity [

	^ #error
]

{ #category : 'initialization' }
ReIconHardcodedMessageRule >> initialize [
	super initialize.
	self
		replace: 'Smalltalk ui icons `iconName'
		byEvaluating: [ :node :matchMap |
			OCParser
				parseExpression: 'self iconNamed: #' , (matchMap at: #'`iconName') ]
]
