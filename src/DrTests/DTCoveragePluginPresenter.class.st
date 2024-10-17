"
I am a specialized `DTDefaultPluginPresenter` for the DrTests coverage plugin.

This subclass adds an extra panel to display the source code of the methods and highlight the covered lines of code. The highlighting has 3 colors where:
- green indicates fully covered lines.
- yellow indicates partially covered lines.
- red indicates uncovered lines.

To achieve this, this subclass has 4 new attributes: sourceCodePanel, coveredNodes, uncoveredNodes, and yellowNodes. The sourceCodePanel is the panel that displays the highlighted source code below the list of method nodes. It also has 4 new methods:
1) `DTCoveragePluginPresenter>>#updateSourceCodePanel:` updates the panel when the user clicks on a method in the result list. It colors in the order they are presented above, starting from green as a base color.
2) `DTCoveragePluginPresenter>>#defineColorCoverage:` updates the coveredNodes and uncoveredNodes attributes according to the selected method. 
3) `DTCoveragePluginPresenter>>#defineColorMessageNode:` updates the yellowNodes attribute according to the selected method. If an ASTMessageNode contains selectors with block arguments like `ifTrue:`, `ifFalse:`, `do:` and others, it decides whether they should be highlighted in yellow or green.
4) `DTCoveragePluginPresenter>>#addHighlightingOf:withColor:` applies the highlighting to the source code panel for each attribute (coveredNodes, uncoveredNodes, and yellowNodes).
"
Class {
	#name : 'DTCoveragePluginPresenter',
	#superclass : 'DTDefaultPluginPresenter',
	#instVars : [
		'sourceCodePanel',
		'coveredNodes',
		'uncoveredNodes',
		'yellowNodes'
	],
	#category : 'DrTests-Spec',
	#package : 'DrTests',
	#tag : 'Spec'
}

{ #category : 'highlighting' }
DTCoveragePluginPresenter >> addHighlightingOf: nodesCollection withColor: aColor [

	nodesCollection do: [ :node |
		sourceCodePanel addTextSegmentDecoration:
			(SpTextPresenterDecorator forHighlight
				 interval:
					 (node sourceInterval first to: node sourceInterval last + 1);
				 highlightColor: aColor) ]
]

{ #category : 'initialization' }
DTCoveragePluginPresenter >> cleanAttributesForHighlighting [
	" Clean the attributes to store new value to display on Source Code panel "

	yellowNodes := OrderedCollection new.
	uncoveredNodes := OrderedCollection new.
	coveredNodes := OrderedCollection new
]

{ #category : 'initialization' }
DTCoveragePluginPresenter >> connectPresenters [

	super connectPresenters.

	self resultViewsDropList whenSelectedItemChangedDo: [
		:resultTreeViewOrNil |
		resultTreeViewOrNil ifNotNil: [
			resultsList roots:
				(resultTreeViewOrNil resultTreeFor: drTests pluginResult)
					subResults ] ].

	resultsList whenSelectionChangedDo: [ :selection |
		(selection selectedItem isKindOf: DTTestLeafNode) ifTrue: [
			selection selectedItem ifNotNil: [ :item |
				self updateSourceCodePanel: item content ] ] ]
]

{ #category : 'layout' }
DTCoveragePluginPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		  add: (SpPanedLayout newLeftToRight
				   add: (SpPanedLayout newLeftToRight
						    add: packagesList;
						    add: itemsList;
						    yourself);
				   add: (SpBoxLayout newTopToBottom
						    spacing: 5;
						    add: resultViewsDropList expand: false;
						    add: resultLabel expand: false;
						    add: resultsList;
						    add: sourceCodePanel;
						    yourself);
				   yourself);
		  add: startButton expand: false;
		  yourself
]

{ #category : 'highlighting' }
DTCoveragePluginPresenter >> defineColorCoverage: aMethod [

	aMethod ast nodesDo: [ :node |
		node isSequence ifTrue: [
			node hasBeenExecuted
				ifTrue: [
					(self hasSequenceNodeInside: node) ifFalse: [
						coveredNodes add: node ] ]
				ifFalse: [ uncoveredNodes add: node ] ].

		node isMessage ifTrue: [ self defineColorMessageNode: node ] ]
]

{ #category : 'highlighting' }
DTCoveragePluginPresenter >> defineColorMessageNode: aMessageNode [
	"handle arguments of an ASTMessageNode"

	| blockNodes |
	blockNodes := aMessageNode arguments select: [ :element |
		              element isBlock ].
	blockNodes ifNotEmpty: [
		(blockNodes allSatisfy: [ :arg | arg body hasBeenExecuted ])
			ifFalse: [ yellowNodes add: aMessageNode . ] ]
]

{ #category : 'highlighting' }
DTCoveragePluginPresenter >> hasSequenceNodeInside: aSequenceNode [

	^ aSequenceNode statements anySatisfy: [ :statement |
		  statement allChildren anySatisfy: [ :node | node isSequence ] ]
]

{ #category : 'initialization' }
DTCoveragePluginPresenter >> initializeResultsTreeAndLabel [

	resultLabel := self newLabel label: 'Results:'.
	resultsList := self newTree.
	resultsList
		display: [ :node | node drTestsName ];
		displayColor: [ :node | node displayColor ];
		children: [ :node | node subResults ];
		actions: self rootCommandsGroup / 'Results tools'.

	resultsList outputActivationPort transmitDo: [
		(DTResultBrowseCommand forSpecContext: self) execute ].
	sourceCodePanel := self instantiate: SpCodePresenter .
]

{ #category : 'layout' }
DTCoveragePluginPresenter >> updateSourceCodePanel: compiledMethod [
	" The colors overlap and paint in descending order"

	self cleanAttributesForHighlighting.
	self defineColorCoverage: compiledMethod.
	sourceCodePanel beForMethod: compiledMethod.
	sourceCodePanel text: compiledMethod sourceCode.
	self
		addHighlightingOf: coveredNodes
		withColor: (Color r: 0.74 g: 0.98 b: 0.71).
	self
		addHighlightingOf: uncoveredNodes
		withColor: (Color r: 0.98 g: 0.71 b: 0.71).
	self
		addHighlightingOf: yellowNodes
		withColor: (Color r: 0.98 g: 0.95 b: 0.71).

	"Base color is green"
	sourceCodePanel addTextSegmentDecoration:
		(SpTextPresenterDecorator forHighlight
			 interval: (compiledMethod ast body sourceInterval first to:
					  compiledMethod ast body sourceInterval last + 1);
			 highlightColor: (Color r: 0.74 g: 0.98 b: 0.71)).
	^ sourceCodePanel yourself
]
