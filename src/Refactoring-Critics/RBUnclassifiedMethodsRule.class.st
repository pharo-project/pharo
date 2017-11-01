"
All methods should be put into a protocol (method category) for better readability.
"
Class {
	#name : #RBUnclassifiedMethodsRule,
	#superclass : #RBBlockLintRule,
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBUnclassifiedMethodsRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBUnclassifiedMethodsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnclassifiedMethodsRule'
]

{ #category : #running }
RBUnclassifiedMethodsRule >> basicCheck: aMethod [ 

	^ aMethod protocol = Protocol unclassified 
		
]

{ #category : #running }
RBUnclassifiedMethodsRule >> checkMethod: aMethod [ 

	(self basicCheck: aMethod ) 
		ifTrue: [ result addMethod: aMethod ]
]

{ #category : #accessing }
RBUnclassifiedMethodsRule >> group [
	^ 'Style'
]

{ #category : #accessing }
RBUnclassifiedMethodsRule >> name [
	^ 'Unclassified methods'
]
