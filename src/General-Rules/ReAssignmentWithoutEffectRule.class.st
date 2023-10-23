"
This smell arises when a statement such as x := x is found. This statement has not effect, it can be removed.
"
Class {
	#name : 'ReAssignmentWithoutEffectRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReAssignmentWithoutEffectRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'AssignmentWithoutEffectRule'
]

{ #category : 'accessing' }
ReAssignmentWithoutEffectRule >> group [
	^ 'Optimization'
]

{ #category : 'initialization' }
ReAssignmentWithoutEffectRule >> initialize [
	super initialize.
	self  replace: 	'`var := `var' with: ''
]

{ #category : 'accessing' }
ReAssignmentWithoutEffectRule >> name [
	^ 'Assignment has no effect'
]

{ #category : 'accessing' }
ReAssignmentWithoutEffectRule >> severity [
	^ #information
]
