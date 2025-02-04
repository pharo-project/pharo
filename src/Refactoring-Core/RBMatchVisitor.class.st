"
I am a visitor to change temporaries and arguments to generic names (`tempMatch , `argMatch)
"
Class {
	#name : 'RBMatchVisitor',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'arguments',
		'temporaries',
		'difference'
	],
	#category : 'Refactoring-Core-Base',
	#package : 'Refactoring-Core',
	#tag : 'Base'
}

{ #category : 'accessing' }
RBMatchVisitor >> difference [
	^ difference
]

{ #category : 'initialization' }
RBMatchVisitor >> initialize [
	super initialize .
	arguments := 0 .
	temporaries := 0.
	difference := 0
]

{ #category : 'accessing' }
RBMatchVisitor >> replace: temp with: aString [
	|definingNode|
	definingNode := temp whoDefines: temp name.
	(OCParseTreeRewriter rename: temp name to: aString) executeTree: definingNode
]

{ #category : 'visiting' }
RBMatchVisitor >> visitBlockNode: aBlockNode [
	aBlockNode arguments do: [ :arg |
		self replace: arg with: ('tempMatch', temporaries  asString).
		temporaries := temporaries + 1 ].
	super visitBlockNode: aBlockNode
]

{ #category : 'visiting' }
RBMatchVisitor >> visitMethodNode: aMethodNode [
	aMethodNode arguments do: [ :arg |
		difference := difference + (('argMatch', arguments  asString )size- (arg name asString )size + 2 ).
		self replace: arg with: ('argMatch', arguments  asString).
		arguments := arguments + 1 ].
	super visitMethodNode: aMethodNode
]

{ #category : 'visiting' }
RBMatchVisitor >> visitSequenceNode: aSequenceNode [
	|isMethod|
	isMethod := aSequenceNode parent isMethod.
	aSequenceNode temporaries do: [ :temp |
		isMethod ifTrue: [
			difference := difference + (('tempMatch', temporaries  asString ) size- (temp name asString ) size + 2 )].
		self replace: temp with: ('tempMatch' , temporaries  asString).
		temporaries := temporaries + 1].
	super visitSequenceNode: aSequenceNode
]
