"
This is a simple browser that displays the rb tree and previews each node on the source code.

Example:
self openOn: (self>>#compose) parseTree
"
Class {
	#name : #GTSimpleRBTreeBrowser,
	#superclass : #GLMCompositePresentation,
	#category : #'GT-InspectorExtensions-Core'
}

{ #category : #building }
GTSimpleRBTreeBrowser >> compose [
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
GTSimpleRBTreeBrowser >> sourceIn: a [
	^ a pharoMethod 
		smalltalkClass: [ :each | each parents first methodClass ]; 
		display: [ :each | each source ]
]

{ #category : #building }
GTSimpleRBTreeBrowser >> treeIn: a [
	^ a tree
		rootsExpanded;
		display: [ :anObject | {anObject} ];
		children: [ :each | each children ];
		selectionPopulate: #selectedNode
			on: $o
			entitled: 'Open'
			with: [ :tree | tree selection ];
		format: [ :each | 
					String
						streamContents: [ :stream | 
							stream
								nextPutAll: each class name;
								nextPut: $(;
								nextPutAll: ((each formattedCode truncateTo: 50) copyReplaceAll: String cr with: String space);
								nextPut: $) ] ]
]
