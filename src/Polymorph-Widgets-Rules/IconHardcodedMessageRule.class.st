"
According to the new API you should use

	#iconName asIcon
	
Instead of

	Smalltalk ui icons iconName
"
Class {
	#name : #IconHardcodedMessageRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'Polymorph-Widgets-Rules'
}

{ #category : #accessing }
IconHardcodedMessageRule >> group [

	^ 'API Change'
]

{ #category : #initialization }
IconHardcodedMessageRule >> initialize [
	super initialize.
	self
		replace: 'Smalltalk ui icons `iconName'
		byEvaluating: [ :node :matchMap | 
			RBParser
				parseExpression: 'self iconNamed: #' , (matchMap at: #'`iconName') ]
]

{ #category : #accessing }
IconHardcodedMessageRule >> name [

	^ 'Use self iconNamed: #symbol instead of asIcon or Smalltalk ui icons iconNamed: #symbol'
]

{ #category : #accessing }
IconHardcodedMessageRule >> severity [

	^ #error
]
