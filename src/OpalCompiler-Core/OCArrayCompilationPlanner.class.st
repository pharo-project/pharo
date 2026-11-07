Class {
	#name : 'OCArrayCompilationPlanner',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'base',
		'limit'
	],
	#category : 'OpalCompiler-Core-Translator',
	#package : 'OpalCompiler-Core',
	#tag : 'Translator'
}

{ #category : 'visiting' }
OCArrayCompilationPlanner >> limit: anInteger [

	limit := anInteger.
	base := 0
]

{ #category : 'visiting' }
OCArrayCompilationPlanner >> visitArrayNode: anArrayNode [

	base + (OCStackDepthVisitor new visitNode: anArrayNode) <= limit
		ifTrue: [
				anArrayNode removeProperty: #compileAsLargeArray ifAbsent: [ ].
				self visitPushed: anArrayNode statements from: base ]
		ifFalse: [
				anArrayNode propertyAt: #compileAsLargeArray put: true.
				anArrayNode statements do: [ :each | self visitPushed: { each } from: base + 3 ] ]
]

{ #category : 'visiting' }
OCArrayCompilationPlanner >> visitBlockNode: aBlockNode [
	"A full block has its own frame and is planned when it is translated"

	aBlockNode isInlined ifTrue: [ self visitNode: aBlockNode body ]
]

{ #category : 'visiting' }
OCArrayCompilationPlanner >> visitCascadeNode: aCascadeNode [

	self visitNode: aCascadeNode receiver.
	aCascadeNode messages do: [ :message | self visitPushed: message arguments from: base + 2 ]
]

{ #category : 'visiting' }
OCArrayCompilationPlanner >> visitMessageNode: aMessageNode [

	self visitPushed: { aMessageNode receiver } , aMessageNode arguments from: base
]

{ #category : 'visiting' }
OCArrayCompilationPlanner >> visitPushed: aCollectionOfNodes from: aBase [

	| previousBase |
	previousBase := base.
	aCollectionOfNodes withIndexDo: [ :node :index |
			base := aBase + index - 1.
			self visitNode: node ].
	base := previousBase
]

{ #category : 'visiting' }
OCArrayCompilationPlanner >> visitSequenceNode: aSequenceNode [

	aSequenceNode statements do: [ :each | self visitNode: each ]
]
