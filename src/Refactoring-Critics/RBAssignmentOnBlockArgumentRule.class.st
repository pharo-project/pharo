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
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
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
RBAssignmentOnBlockArgumentRule >> basicCheck: aMethod [
	^ (RBFindBlockArgumentAssignment collectAssignments: aMethod ast) notEmpty 
		
]

{ #category : #running }
RBAssignmentOnBlockArgumentRule >> checkMethod: aMethod [
	(RBFindBlockArgumentAssignment collectAssignments: aMethod ast)
		do: [ :item | 
			result addMethod: aMethod.
			result addSearchString: item ]
]

{ #category : #accessing }
RBAssignmentOnBlockArgumentRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBAssignmentOnBlockArgumentRule >> name [
	^ 'Assignments on block arguments'
]

{ #category : #accessing }
RBAssignmentOnBlockArgumentRule >> rationale [
	^ 'Check for assignments on block arguments.
For example:

[:x :y|
	x:= x+y.
	]

The block argument "x" should not be written'
]
