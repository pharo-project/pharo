"
See my #longDescription .
"
Class {
	#name : #RBRemoveAssignmentWithoutEffectRule,
	#superclass : #ReNodeRewriteRule,
	#category : 'GeneralRules-Migrated'
}

{ #category : #accessing }
RBRemoveAssignmentWithoutEffectRule class >> uniqueIdentifierName [

	^ 'RemoveAssignmentWithoutEffectRule'
]

{ #category : #accessing }
RBRemoveAssignmentWithoutEffectRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBRemoveAssignmentWithoutEffectRule >> initialize [
	super initialize.
 
	self
		replace: 	'`var := `var' with: ''
]

{ #category : #'as yet unclassified' }
RBRemoveAssignmentWithoutEffectRule >> longDescription [ 
	^ 'Remove assignment has no effect. For example, var := var is unless'
]

{ #category : #accessing }
RBRemoveAssignmentWithoutEffectRule >> name [
	^ 'Remove assignment has no effect'
]
