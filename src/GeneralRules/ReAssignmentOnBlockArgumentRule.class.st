"
Check for assignments on method and block arguments.
For example:

[:x :y|
	x:= x+y.
	]

The block argument ""x"" should not be written. This is considered bad style. And some compiler may reject that code.

Same for method arguments, you should not write those either.
"
Class {
	#name : #ReAssignmentOnBlockArgumentRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReAssignmentOnBlockArgumentRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReAssignmentOnBlockArgumentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ 'AssignmentOnBlockArgumentRule'
]

{ #category : #running }
ReAssignmentOnBlockArgumentRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemAssignments|
	problemAssignments := aMethod ast assignmentNodes 
		select: [ :assignment | assignment variable isArg].
	
	problemAssignments do: [ :assignment |
			aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: assignment hint: assignment variable sourceCode)]
]

{ #category : #accessing }
ReAssignmentOnBlockArgumentRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReAssignmentOnBlockArgumentRule >> name [
	^ 'Assignments on block arguments'
]
