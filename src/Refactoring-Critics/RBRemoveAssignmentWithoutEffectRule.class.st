"
See my #longDescription .
"
Class {
	#name : #RBRemoveAssignmentWithoutEffectRule,
	#superclass : #RBTransformationRule,
	#category : #Refactoring-Critics
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
 
	self rewriteRule
		replace: 	'`var := `var' with: ''
]

{ #category : #accessing }
RBRemoveAssignmentWithoutEffectRule >> name [
	^ 'Remove assignment has no effect'
]

{ #category : #accessing }
RBRemoveAssignmentWithoutEffectRule >> rationale [ 
	^ 'Remove assignment has no effect. For example, var := var is unless'
]
