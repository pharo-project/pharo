"
Check for assignments on block arguments.
For example:

[:x :y|
	x:= x+y.
	]

The block argument ""x"" should not be written. This is considered bad style. And some compiler may reject that code.
"
Class {
	#name : #RBAssignmentOnBlockArgumentRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBAssignmentOnBlockArgumentRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBAssignmentOnBlockArgumentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ 'AssignmentOnBlockArgumentRule'
]

{ #category : #running }
RBAssignmentOnBlockArgumentRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemAssignments|
	problemAssignments := RBFindBlockArgumentAssignment collectAssignments: aMethod ast.
	
	problemAssignments do: [ :assignment |
			aCriticBlock cull: (ReTrivialCritique  
			withAnchor: (ReIntervalSourceAnchor
				entity: aMethod
				interval: assignment sourceInterval)
			by: self
			hint: assignment variable sourceCode)]
]

{ #category : #accessing }
RBAssignmentOnBlockArgumentRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBAssignmentOnBlockArgumentRule >> name [
	^ 'Assignments on block arguments'
]
