"
Checks for instance variables that might better be defined as temporary variables. If an instance variable is only used in one method and it is always assigned before it is used, then that method could define that variable as a temporary variable of the method instead (assuming that the method is not recursive).
"
Class {
	#name : #RBVariableReferencedOnceRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBVariableReferencedOnceRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBVariableReferencedOnceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'VariableReferencedOnceRule'
]

{ #category : #running }
RBVariableReferencedOnceRule >> basicCheck: aClass [
	^ aClass instVarNames
		anySatisfy: [ :each | 
			| defClass selector |
			(aClass withAllSubclasses
				inject: 0
				into: [ :sum :class | 
					| sels |
					sels := class whichSelectorsAccess: each.
					sels size == 1
						ifTrue: [ selector := sels asArray first.
							defClass := class ].
					sum + sels size ]) == 1
				and: [ | tree |
					tree := defClass parseTreeFor: selector.
					tree notNil and: [ RBReadBeforeWrittenTester isVariable: each writtenBeforeReadIn: tree ] ] ]
]

{ #category : #accessing }
RBVariableReferencedOnceRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBVariableReferencedOnceRule >> name [
	^ 'Variable referenced in only one method and always assigned first'
]

{ #category : #accessing }
RBVariableReferencedOnceRule >> severity [
	^ #information
]
