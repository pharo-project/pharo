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
				parseExpression: '#' , (matchMap at: #'`iconName') , ' asIcon' ]
]

{ #category : #accessing }
IconHardcodedMessageRule >> name [

	^ 'Use #iconName asIcon'
]

{ #category : #accessing }
IconHardcodedMessageRule >> severity [

	^ #error
]
