"
For proper versioning, every method should have a timestamp.
"
Class {
	#name : #RBMethodHasNoTimeStampRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
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
RBMethodHasNoTimeStampRule >> basicCheck: aMethod [ 
	^ aMethod timeStamp isEmpty
]

{ #category : #running }
RBMethodHasNoTimeStampRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) ifTrue: 
		[ result addMethod: aMethod ]
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
