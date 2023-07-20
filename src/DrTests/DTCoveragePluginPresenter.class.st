"
This inherits from `DTDefaultPluginPresenter`.

This subclass adds an extra panel to display the source code of the methods and highlight the covered code lines. The highlighting has 3 colors where:
- green indicates fully covered lines.
- yellow indicates partially covered lines.
- red indicates non-covered lines.

To achieve this, this subclass has 4 new attributes: sourceCodePanel, coveredNodes, uncoveredNodes, and yellowNodes. Where sourceCodePanel is the panel that displays de source code highlighted and the rest list of nodes that has a method. Furthermore,  it has 4 new methods:
1) `DTDefaultPluginPresenter>>#updateSourceCodePanel:` It updated the panel when the user click a method on the result list.
2) `DTDefaultPluginPresenter>>#defineColorCoverage:` According to the selected method, it updates the coveredNodes and uncoveredNodes attributes. 
3) `DTDefaultPluginPresenter>>#defineColorMessageNode:` According to the selected method, it updated the yellowNodes attribute. Because RBMessageNode is a complex node that has a selector like IfTrue, IfFalse, do, and others. It helps to define if those selectors should be yellow o green highlighting.
4)  `DTDefaultPluginPresenter>>#getSubIntervalsOf:from:` helps to define the positions of the selector of MessageNode to paint yellow.
"
Class {
	#name : #DTCoveragePluginPresenter,
	#superclass : #DTDefaultPluginPresenter,
	#instVars : [
		'sourceCodePanel',
		'coveredNodes',
		'uncoveredNodes',
		'yellowNodes'
	],
	#category : #'DrTests-Spec'
}

{ #category : #initialization }
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

{ #category : #layout }
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

{ #category : #'as yet unclassified' }
DTCoveragePluginPresenter >> defineColorCoverage: aMethod [

	aMethod ast nodesDo: [ :node |
		node isSequence ifTrue: [
			node hasBeenExecuted
				ifTrue: [ coveredNodes add: node ]
				ifFalse: [ uncoveredNodes add: node ] ].

		node isMessage ifTrue: [ self defineColorMessageNode: node ] ]
]

{ #category : #'as yet unclassified' }
DTCoveragePluginPresenter >> defineColorMessageNode: aRBMessageNode [
	"handle arguments of a RBMessageNode"

	| blockNodes |
	"handle arguments"
	blockNodes := aRBMessageNode arguments select: [ :element |
		              element isKindOf: RBBlockNode ].
	blockNodes isNotEmpty ifTrue: [
		(blockNodes allSatisfy: [ :arg | arg body hasBeenExecuted ])
			ifFalse: [
				| nodeInterval nodeSubIntervals resultInterval |
				nodeSubIntervals := OrderedCollection new.
				nodeInterval := aRBMessageNode sourceInterval.
				aRBMessageNode arguments do: [ :node |
					nodeSubIntervals add: node sourceInterval ].
				resultInterval := self
					                  getSubIntervalsOf: nodeInterval
					                  from: nodeSubIntervals.
				yellowNodes addAll: resultInterval ] ]
]

{ #category : #'as yet unclassified' }
DTCoveragePluginPresenter >> getSubIntervalsOf: intervalNode from: subIntervalNodes [

	| resultArray previousEnd |
	resultArray := OrderedCollection new.
	previousEnd := intervalNode first - 2.
	subIntervalNodes do: [ :subInterval |
		| start end |
		start := subInterval first.
		end := subInterval last.

		start - previousEnd > 1 ifTrue: [
			resultArray add: (previousEnd + 1 to: start - 1) ].
		previousEnd := end ].

	previousEnd < intervalNode last ifTrue: [
		resultArray add: (previousEnd + 1 to: intervalNode last) ].

	resultArray := resultArray asOrderedCollection.

	^ resultArray
]

{ #category : #initialization }
DTCoveragePluginPresenter >> initializeResultsTreeAndLabel [

	resultLabel := self newLabel label: 'Results:'.
	resultsList := self newTree.
	resultsList
		display: [ :node | node drTestsName ];
		displayColor: [ :node | node displayColor ];
		children: [ :node | node subResults ];
		contextMenu: [
			(self rootCommandsGroup / 'Results tools') beRoot asMenuPresenter ].

	resultsList outputActivationPort transmitDo: [
		(DTResultBrowseCommand forSpecContext: self) execute ].
	sourceCodePanel := self instantiate: SpCodePresenter
]

{ #category : #'as yet unclassified' }
DTCoveragePluginPresenter >> updateSourceCodePanel: compiledMethod [

	yellowNodes := OrderedCollection new.
	uncoveredNodes := OrderedCollection new.
	coveredNodes := OrderedCollection new.

	self defineColorCoverage: compiledMethod.

	sourceCodePanel beForMethod: compiledMethod.
	sourceCodePanel text: compiledMethod sourceCode.

	uncoveredNodes do: [ :node |
		sourceCodePanel addTextSegmentDecoration:
			(SpTextPresenterDecorator forHighlight
				 interval:
					 (node sourceInterval first to: node sourceInterval last + 1);
				 highlightColor: (Color r: 0.98 g: 0.71 b: 0.71)) ].

	yellowNodes do: [ :interval |
		sourceCodePanel addTextSegmentDecoration:
			(SpTextPresenterDecorator forHighlight
				 interval: interval + 1;
				 highlightColor: (Color r: 0.98 g: 0.95 b: 0.71)) ].

	coveredNodes do: [ :node |
		sourceCodePanel addTextSegmentDecoration:
			(SpTextPresenterDecorator forHighlight
				 interval:
					 (node sourceInterval first to: node sourceInterval last + 1);
				 highlightColor: (Color r: 0.74 g: 0.98 b: 0.71)) ].

	^ sourceCodePanel yourself
]
