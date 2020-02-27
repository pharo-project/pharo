Class {
	#name : #RGUnknownSlot,
	#superclass : #RGSlot,
	#instVars : [
		'expression'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'default model values' }
RGUnknownSlot >> defaultExpression [

	'InstanceVariableSlot named: #slotOfUnknownType'
]

{ #category : #'private - backend interface' }
RGUnknownSlot >> definitionString [

	^ String streamContents: [ :aStream |
		aStream nextPutAll: '#'; nextPutAll: self name; nextPutAll: ' => '; nextPutAll: self expression ]
]

{ #category : #'accessing - backend' }
RGUnknownSlot >> expression [

	^ self backend forBehavior expressionFor: self
]

{ #category : #'accessing - backend' }
RGUnknownSlot >> expression: aString [

	self parent parent announceDefinitionChangeDuring: [ 
		self backend forBehavior setExpressionFor: self to: aString ].


]

{ #category : #initialization }
RGUnknownSlot >> initialize [

	super initialize.
	
	expression := self unresolvedValue: self defaultExpression.

]

{ #category : #initialization }
RGUnknownSlot >> initializeUnresolved [

	super initializeUnresolved.
	
	expression := self unresolvedValue: self defaultExpression.
]

{ #category : #resolving }
RGUnknownSlot >> makeResolved [

	super makeResolved.
	
	expression := self expression markAsRingResolved.

]

{ #category : #'private - backend interface' }
RGUnknownSlot >> pvtExpression [

	^ expression value
]

{ #category : #'private - backend interface' }
RGUnknownSlot >> pvtExpression: aString [

	^ expression := aString
]

{ #category : #'private - backend interface' }
RGUnknownSlot >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#expression -> expression. 
	}
	
	
]
