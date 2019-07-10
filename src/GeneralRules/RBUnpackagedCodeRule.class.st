"
Code that is not contained in a Monticello package is not versioned and cannot be brought into a different image.
"
Class {
	#name : #RBUnpackagedCodeRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBUnpackagedCodeRule class >> checksClass [
	^ true
]

{ #category : #testing }
RBUnpackagedCodeRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBUnpackagedCodeRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnpackagedCodeRule'
]

{ #category : #running }
RBUnpackagedCodeRule >> basicCheck: anEntity [
	^ anEntity package isNotNil and: [ anEntity package isDefault ]
]

{ #category : #accessing }
RBUnpackagedCodeRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBUnpackagedCodeRule >> name [
	^ 'Unpackaged code'
]
