"
Find assignment nodes with block arguments on its left side.
"
Class {
	#name : #RBFindBlockArgumentAssignment,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'arguments',
		'argumentAssignments'
	],
	#category : #AST-Core-Visitors
}

{ #category : #accessing }
RBFindBlockArgumentAssignment class >> collectAssignments: aParseTree [
	^ (self new executeTree: aParseTree) assignments
]

{ #category : #accessing }
RBFindBlockArgumentAssignment >> assignments [
	^  argumentAssignments
]

{ #category : #visiting }
RBFindBlockArgumentAssignment >> executeTree: aParseTree [
	^ self visitNode: aParseTree
]

{ #category : #initialization }
RBFindBlockArgumentAssignment >> initialize [
	super initialize.
	arguments := Set new.
	argumentAssignments := Set new.

]

{ #category : #private }
RBFindBlockArgumentAssignment >> is: aVar inScopeOf: anArgumentNode [
	"if this Var is a child of if anArgumentNode parent, then the
assignment on aVar happens in scope of the BlockNode of this ArgumentNode "

	| parent |
	parent := aVar parent.
	[ parent notNil ]
		whileTrue: [ 
			parent = anArgumentNode parent
				ifTrue: [ ^ true ].
			parent := parent parent ].
	^ false
]

{ #category : #visiting }
RBFindBlockArgumentAssignment >> visitArgumentNode: anArgumentNode [
	"register argument node if it is an block argument"

	anArgumentNode parent isBlock
		ifTrue: [ arguments add: anArgumentNode ]
]

{ #category : #visiting }
RBFindBlockArgumentAssignment >> visitAssignmentNode: anAssignmentNode [
	"search registered argument variable with the same name and scope"

	| varName |
	super visitAssignmentNode: anAssignmentNode.
	varName := anAssignmentNode variable name.
	arguments
		detect: [ :elem | elem name = varName and: [ self is: anAssignmentNode variable inScopeOf: elem ] ]
		ifFound: [ argumentAssignments add: varName ]
]

{ #category : #visiting }
RBFindBlockArgumentAssignment >> visitTemporaryNodes: aNodeCollection [
	"here our trait calls visitArgumentNodes: but we are not interested on temporary nodes -> ignore this."
	
]
