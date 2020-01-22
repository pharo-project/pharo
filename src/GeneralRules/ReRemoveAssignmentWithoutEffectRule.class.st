"
See my #longDescription .
"
Class {
	#name : #ReRemoveAssignmentWithoutEffectRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReRemoveAssignmentWithoutEffectRule class >> uniqueIdentifierName [

	^ 'RemoveAssignmentWithoutEffectRule'
]

{ #category : #accessing }
ReRemoveAssignmentWithoutEffectRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
ReRemoveAssignmentWithoutEffectRule >> initialize [
	super initialize.
 
	self
		replace: 	'`var := `var' with: ''
]

{ #category : #accessing }
ReRemoveAssignmentWithoutEffectRule >> name [
	^ 'Remove assignment has no effect'
]

{ #category : #'as yet unclassified' }
ReRemoveAssignmentWithoutEffectRule >> rationale [ 
	^ 'Remove assignment has no effect. For example, var := var is unless'
]
