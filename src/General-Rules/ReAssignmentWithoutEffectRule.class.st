"
This smell arises when a statement such as x := x is found. This statement has not effect, it can be removed.
"
Class {
	#name : 'ReAssignmentWithoutEffectRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReAssignmentWithoutEffectRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReAssignmentWithoutEffectRule class >> ruleName [
	^ 'Assignment has no effect'
]

{ #category : 'accessing' }
ReAssignmentWithoutEffectRule class >> severity [
	^ #information
]

{ #category : 'accessing' }
ReAssignmentWithoutEffectRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'AssignmentWithoutEffectRule'
]

{ #category : 'initialization' }
ReAssignmentWithoutEffectRule >> initialize [
	super initialize.
	self  replace: 	'`var := `var' with: ''
]
