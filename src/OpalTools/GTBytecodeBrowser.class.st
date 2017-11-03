"
This is a simple browser that displays bytecode and previews each node on the source code.

Example:
self openOn: (self>>#compose) 
"
Class {
	#name : #GTBytecodeBrowser,
	#superclass : #GLMCompositePresentation,
	#category : #OpalTools
}

{ #category : #building }
GTBytecodeBrowser >> compose [
	self title: 'Tree'.
	self tabulator with: [ :browser |
		browser
			row: #tree;
			row: #source.
		browser transmit
			to: #tree;
			andShow: [ :a | self treeIn: a ].
		browser transmit
			to: #source;
			andShow: [ :a | self sourceIn: a ].
		browser transmit
			from: #tree;
			to: #source port: #selectionInterval;
			transformed: [ :rbNode | rbNode sourceInterval ].
		browser transmit
			toOutsidePort: #selection;
			from: #tree port: #selectedNode ]
]

{ #category : #building }
GTBytecodeBrowser >> sourceIn: a [
	^ a pharoMethod 
		smalltalkClass: [ :each | each method methodClass ]; 
		display: [ :each | each sourceCode ]
]

{ #category : #building }
GTBytecodeBrowser >> treeIn: a [
	^ a tree
		rootsExpanded;
		display: [ :anObject | anObject symbolicBytecodes ];
		children: [ :each | each isArray ifTrue: [each] ifFalse: #() ];
		selectionPopulate: #selectedNode
			on: $o
			entitled: 'Open'
			with: [ :tree | tree selection ];
		format: [ :each | each printString].
]
