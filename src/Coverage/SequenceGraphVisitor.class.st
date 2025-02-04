"
A Pharo AST visitor used internally to compute the callgraph of nodes.

See `CoverageCollector>>#callgraph` for details.
"
Class {
	#name : 'SequenceGraphVisitor',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'edges',
		'currentNode',
		'collector'
	],
	#category : 'Coverage-Base',
	#package : 'Coverage',
	#tag : 'Base'
}

{ #category : 'accessing' }
SequenceGraphVisitor >> collector [

	^ collector
]

{ #category : 'accessing' }
SequenceGraphVisitor >> collector: anObject [

	collector := anObject
]

{ #category : 'accessing' }
SequenceGraphVisitor >> edges [

	^ edges
]

{ #category : 'accessing' }
SequenceGraphVisitor >> edges: anObject [

	edges := anObject
]

{ #category : 'initialization' }
SequenceGraphVisitor >> initialize [

	edges := OrderedCollection new.
]

{ #category : 'visiting' }
SequenceGraphVisitor >> visitMessageNode: aMessageNode [

	"Visit recusively"
	super visitMessageNode: aMessageNode.

	"And add an adge to all "
	collector methods
		select: [ :m | m selector = aMessageNode selector ]
		thenDo: [ :m |
			| target |
			target := m ast body.
			self assert: target isSequence.
			edges add: { currentNode. target }. ].

]

{ #category : 'visiting' }
SequenceGraphVisitor >> visitSequenceNode: aSequenceNode [

	"Dot not visit sub-sequences, just mark an edge"
	currentNode ifNotNil: [
		edges add: { currentNode. aSequenceNode }.
		^ self ].
	
	currentNode := aSequenceNode.
	super visitSequenceNode: aSequenceNode.
	currentNode := nil.


]
