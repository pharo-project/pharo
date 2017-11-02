"
A critic which represents that there is an issue with the order of messages being sent
"
Class {
	#name : #ReInvocationOrderCritique,
	#superclass : #ReAbstractCritique,
	#instVars : [
		'targetMethodName',
		'condition'
	],
	#category : 'Renraku-Critiques'
}

{ #category : #'instance creation' }
ReInvocationOrderCritique class >> withAnchor: anAnchor by: aRule method: aString condition: aCondition [

	^ self new
		initializeRule:	aRule
		  sourceAnchor: anAnchor
		        method: aString
		     condition: aCondition;
		
		yourself
]

{ #category : #initialization }
ReInvocationOrderCritique >> initializeRule: aRule sourceAnchor: anAnchor method: aString condition: aCondition [

	self initializeRule: aRule sourceAnchor: anAnchor.
	targetMethodName := aString.
	condition := aCondition 
]

{ #category : #testing }
ReInvocationOrderCritique >> providesChange [
	^ false
]
