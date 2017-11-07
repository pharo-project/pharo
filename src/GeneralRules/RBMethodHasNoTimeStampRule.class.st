"
For proper versioning, every method should have a timestamp.
"
Class {
	#name : #RBMethodHasNoTimeStampRule,
	#superclass : #ReAbstractRule,
	#category : 'GeneralRules-Migrated'
}

{ #category : #testing }
RBMethodHasNoTimeStampRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBMethodHasNoTimeStampRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'MethodHasNoTimeStampRule'
]

{ #category : #running }
RBMethodHasNoTimeStampRule >> check: aMethod forCritiquesDo: aCriticBlock [
	(aMethod timeStamp isEmpty) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]
]

{ #category : #accessing }
RBMethodHasNoTimeStampRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
RBMethodHasNoTimeStampRule >> name [
	^ 'Method has no timeStamp'
]

{ #category : #accessing }
RBMethodHasNoTimeStampRule >> severity [
	^ #error
]
