"
According to the new API you should use

	self iconNamed: #symbol
	
Instead of

	Smalltalk ui icons iconNamed: #symbol
"
Class {
	#name : #ReIconHardcodedMessageRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'Polymorph-Widgets-Rules'
}

{ #category : #accessing }
ReIconHardcodedMessageRule >> group [

	^ 'API Change'
]

{ #category : #initialization }
ReIconHardcodedMessageRule >> initialize [
	super initialize.
	self
		replace: 'Smalltalk ui icons `iconName'
		byEvaluating: [ :node :matchMap | 
			RBParser
				parseExpression: 'self iconNamed: #' , (matchMap at: #'`iconName') ]
]

{ #category : #accessing }
ReIconHardcodedMessageRule >> name [

	^ 'Use self iconNamed: #symbol instead of asIcon or Smalltalk ui icons iconNamed: #symbol'
]

{ #category : #accessing }
ReIconHardcodedMessageRule >> severity [

	^ #error
]
