"
Code that is not contained in a Monticello package is not versioned and cannot be brought into a different image.
"
Class {
	#name : #ReUnpackagedCodeRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReUnpackagedCodeRule class >> checksClass [
	^ true
]

{ #category : #testing }
ReUnpackagedCodeRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReUnpackagedCodeRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnpackagedCodeRule'
]

{ #category : #running }
ReUnpackagedCodeRule >> basicCheck: anEntity [
	^ anEntity package isNotNil and: [ anEntity package isDefault ]
]

{ #category : #accessing }
ReUnpackagedCodeRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReUnpackagedCodeRule >> name [
	^ 'Unpackaged code'
]
