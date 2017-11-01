"
Checks for assignements to temporaries that are not used afterwards.
"
Class {
	#name : #RBUnnecessaryAssignmentRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBUnnecessaryAssignmentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnnecessaryAssignmentRule'
]

{ #category : #accessing }
RBUnnecessaryAssignmentRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBUnnecessaryAssignmentRule >> initialize [
	super initialize.
	self matcher 
		matches: '^`{:aNode | aNode isAssignment and: [(aNode whoDefines: aNode variable name) notNil]}'
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBUnnecessaryAssignmentRule >> name [
	^ 'Unnecessary assignment to a temporary variable'
]

{ #category : #accessing }
RBUnnecessaryAssignmentRule >> severity [
	^ #information
]
