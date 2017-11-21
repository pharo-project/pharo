"
This smell arises when a statement such as x := x is found. This statement has not effect, it can be removed.
"
Class {
	#name : #RBAssignmentWithoutEffectRule,
	#superclass : #ReNodeMatchRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBAssignmentWithoutEffectRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AssignmentWithoutEffectRule'
]

{ #category : #accessing }
RBAssignmentWithoutEffectRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBAssignmentWithoutEffectRule >> initialize [
	super initialize.
	self  matches: '`var := `var'
]

{ #category : #accessing }
RBAssignmentWithoutEffectRule >> name [
	^ 'Assignment has no effect'
]

{ #category : #accessing }
RBAssignmentWithoutEffectRule >> severity [
	^ #information
]
