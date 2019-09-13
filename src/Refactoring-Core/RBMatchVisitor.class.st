"
I am a visitor to change temporaries and arguments to generic names (`tempMatch , `argMatch)
"
Class {
	#name : #RBMatchVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'arguments',
		'temporaries',
		'difference'
	],
	#category : #'Refactoring-Core'
}

{ #category : #accessing }
RBMatchVisitor >> difference [
	^ difference
]

{ #category : #accessing }
RBMatchVisitor >> initialize [ 	
	super initialize .
	arguments := 0 .
	temporaries := 0.
	difference := 0
]

{ #category : #accessing }
RBMatchVisitor >> replace: temp with: aString [
	|definingNode|
	definingNode := temp whoDefines: temp name.
	(RBParseTreeRewriter rename: temp name to: aString) executeTree: definingNode.
]

{ #category : #accessing }
RBMatchVisitor >> visitBlockNode: aBlockNode [
	aBlockNode arguments do: [ :arg | 
		self replace: arg with: ('tempMatch', temporaries  asString).
		temporaries := temporaries + 1 ].
	super visitBlockNode: aBlockNode
]

{ #category : #accessing }
RBMatchVisitor >> visitMethodNode: aMethodNode [
	aMethodNode arguments do: [ :arg | 
		difference := difference + (('argMatch', arguments  asString )size- (arg name asString )size + 2 ).
		self replace: arg with: ('argMatch', arguments  asString).
		arguments := arguments + 1 ].
	super visitMethodNode: aMethodNode.
	
	
]

{ #category : #accessing }
RBMatchVisitor >> visitSequenceNode: aSequenceNode [
	|isMethod|
	isMethod := aSequenceNode parent isMethod.
	aSequenceNode temporaries do: [ :temp |
		isMethod ifTrue: [
			difference := difference + (('tempMatch', temporaries  asString ) size- (temp name asString ) size + 2 )].
		self replace: temp with: ('tempMatch' , temporaries  asString).
		temporaries := temporaries + 1].
	super visitSequenceNode: aSequenceNode.
	
	
]
