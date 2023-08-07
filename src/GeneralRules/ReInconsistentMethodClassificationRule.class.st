"
This smell arises when a method protocol is not equivalent to the one defined in the superclass of such method class. All methods should be put into a protocol (method category) that is equivalent to the one of the superclass, which is a standard style in Pharo. Furthermore, methods which are extended in the superclass are ignored, since they may have different protocol name. Pay attention when you apply automatic recategorisation because it may move method in antoher package if the method is defined in the superclass as an extension.


"
Class {
	#name : #ReInconsistentMethodClassificationRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReInconsistentMethodClassificationRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReInconsistentMethodClassificationRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'InconsistentMethodClassificationRule'
]

{ #category : #running }
ReInconsistentMethodClassificationRule >> check: aMethod forCritiquesDo: aCritiqueBlock [

	| ownerProtocol |
	ownerProtocol := aMethod protocol ifNil: [ ^ self ].

	(ownerProtocol isUnclassifiedProtocol or: [ ownerProtocol isExtensionProtocol ]) ifTrue: [ ^ self ].

	aMethod methodClass superclass ifNotNil: [ :superclass |
		(superclass lookupSelector: aMethod selector) ifNotNil: [ :superMethod |
			| superProtocol |
			superProtocol := superMethod protocol ifNil: [ ^ self ].
			(superProtocol isUnclassifiedProtocol or: [ superProtocol isExtensionProtocol ]) ifTrue: [ ^ self ].
			ownerProtocol name ~= superProtocol name ifTrue: [
				aCritiqueBlock cull: ((self critiqueFor: aMethod) tinyHint: 'superclass categorizes as: ' , superProtocol name) ] ] ]
]

{ #category : #helpers }
ReInconsistentMethodClassificationRule >> critiqueFor: aMethod [

	| protocolInSuperclass |
	protocolInSuperclass := (aMethod methodClass superclass lookupSelector: aMethod selector) protocolName.

	^ (ReRefactoringCritique withAnchor: (self anchorFor: aMethod) by: self) refactoring:
		  (RBMethodProtocolTransformation protocol: { protocolInSuperclass } inMethod: aMethod selector inClass: aMethod methodClass name) asRefactoring
]

{ #category : #accessing }
ReInconsistentMethodClassificationRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
ReInconsistentMethodClassificationRule >> name [
	^ 'Inconsistent method classification'
]

{ #category : #accessing }
ReInconsistentMethodClassificationRule >> severity [
	^ #information
]
