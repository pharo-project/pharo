"
This smell arises when a method protocol is not equivalent to the one defined in the superclass of such method class. All methods should be put into a protocol (method category) that is equivalent to the one of the superclass, which is a standard style in Pharo. Furthermore, methods which are extended in the superclass are ignored, since they may have different protocol name. Pay attention when you apply automatic recategorisation because it may move method in antoher package if the method is defined in the superclass as an extension.


"
Class {
	#name : #RBInconsistentMethodClassificationRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
}

{ #category : #testing }
RBInconsistentMethodClassificationRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBInconsistentMethodClassificationRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'InconsistentMethodClassificationRule'
]

{ #category : #running }
RBInconsistentMethodClassificationRule >> basicCheck: aMethod [ 
	| ownerProtocol |
	ownerProtocol := aMethod protocol.
	ownerProtocol ifNil: [ ^ false ].
	(ownerProtocol == #'as yet unclassified' or: [ 
	 ownerProtocol first = $*	 ]) ifTrue: [ ^ false ].
	
	aMethod methodClass superclass ifNotNil: [ :superclass |
			(superclass lookupSelector: aMethod selector) ifNotNil: [ :superMethod |
				| superProtocol |
				superProtocol := superMethod protocol.
				(superProtocol isNil or: [ 
			 	 superProtocol == #'as yet unclassified' or: [ 
			 	 superProtocol first = $*	 ] ]) ifFalse: [ 
					^ ownerProtocol ~= superProtocol ] ] ].
	
	^ false
]

{ #category : #running }
RBInconsistentMethodClassificationRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) ifTrue: [ 
		result addMethod: aMethod  ]
]

{ #category : #accessing }
RBInconsistentMethodClassificationRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBInconsistentMethodClassificationRule >> name [
	^ 'Inconsistent method classification'
]

{ #category : #accessing }
RBInconsistentMethodClassificationRule >> rationale [
	^ 'All methods should be put into a protocol (method category) that is equivalent to the one of the superclass, which is a Smalltalk style convention.'
]

{ #category : #accessing }
RBInconsistentMethodClassificationRule >> severity [
	^ #information
]
