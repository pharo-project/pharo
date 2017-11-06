"
This is a simple browser that displays the Opal IR and previews each instructions on the source code.

Example:
self openOn: (self>>#compose) ast ir
"
Class {
	#name : #GTIRBrowser,
	#superclass : #GLMCompositePresentation,
	#category : #OpalCompiler-Tools
}

{ #category : #building }
GTIRBrowser >> compose [
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
GTIRBrowser >> sourceIn: a [
	^ a pharoMethod 
		smalltalkClass: [ :each | each method compiledMethod methodClass ]; 
		display: [ :each | each sourceNode source ] 
]

{ #category : #building }
GTIRBrowser >> treeIn: a [
	^ a tree
		rootsExpanded;
		display: [ :anObject | {anObject} ];
		children: [ :each | each children ];
		selectionPopulate: #selectedNode
			on: $o
			entitled: 'Open'
			with: [ :tree | tree selection ];
		format: [ :each | each printString].
]
