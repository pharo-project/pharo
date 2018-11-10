"
I preview the changes that will be introduced in the image when a checkout occurs.

(self onBranch: IceRepository registry anyOne branch) openWithSpec.
"
Class {
	#name : #IceTipCheckoutPreviewBrowser,
	#superclass : #IceTipPreviewBrowser,
	#instVars : [
		'checkoutStrategyLabel',
		'checkoutStrategyList',
		'selectedCheckoutStrategy'
	],
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #specs }
IceTipCheckoutPreviewBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :mainColumn |
			mainColumn
				"newRow: #toolbar height: IceTipToolbar defaultHeight;"
				newRow: [ :row |
					row 
						newColumn: #checkoutStrategyLabel width: 100 * World displayScaleFactor;
						newColumn: #checkoutStrategyList ]
					height: self inputTextHeight;
				newRow: #diffPanel;
				newRow: #actionPanel height: self iceActionPanelHeight ]
]

{ #category : #'instance creation' }
IceTipCheckoutPreviewBrowser class >> onBranch: aBranch [

	^ self on: (IceTipCheckoutModel new
		commitish: aBranch;
		yourself)
]

{ #category : #'accessing ui' }
IceTipCheckoutPreviewBrowser >> checkoutStrategyLabel [
	^ checkoutStrategyLabel
]

{ #category : #'accessing ui' }
IceTipCheckoutPreviewBrowser >> checkoutStrategyLabel: anObject [
	checkoutStrategyLabel := anObject
]

{ #category : #'accessing ui' }
IceTipCheckoutPreviewBrowser >> checkoutStrategyList [
	^ checkoutStrategyList
]

{ #category : #'accessing ui' }
IceTipCheckoutPreviewBrowser >> checkoutStrategyList: anObject [
	checkoutStrategyList := anObject
]

{ #category : #actions }
IceTipCheckoutPreviewBrowser >> doCheckout [
	self model executeCheckoutActionAndThen: [ self window delete ]
]

{ #category : #initialization }
IceTipCheckoutPreviewBrowser >> initializeCheckoutStrategyList [
	| checkoutStrategies |
	checkoutStrategies := self model checkoutStrategies.
	checkoutStrategyList
		items: checkoutStrategies;
		displayBlock: [ :each | each description ].

	checkoutStrategyList whenSelectedItemChanged: [ :aStrategy | 
		self model checkoutStrategy: aStrategy ]
]

{ #category : #initialization }
IceTipCheckoutPreviewBrowser >> initializeWidgets [
	super initializeWidgets.
	
	diffPanel leftLabel: 'Working Copy'.
	diffPanel rightLabel: 'Checkout ' , model targetName.
	
	checkoutStrategyLabel := self newLabel label: 'How to checkout:'.
	checkoutStrategyList := self newDropList.
	self initializeCheckoutStrategyList.
	
	self focusOrder 
		removeAll;
		add: self actionPanel;
		add: self checkoutStrategyList;
		add: self diffPanel
]

{ #category : #'private factory' }
IceTipCheckoutPreviewBrowser >> newActionButton [
	^ self newButton 
		label: 'Checkout';
		icon: (self iconNamed: #branch);
		help: ('Checks out {1} into your image.' format: { self model targetName });
		action: [ self doCheckout ];
		yourself
]

{ #category : #accessing }
IceTipCheckoutPreviewBrowser >> selectedCheckoutStrategy [
	^ selectedCheckoutStrategy
]

{ #category : #accessing }
IceTipCheckoutPreviewBrowser >> title [
	^ 'Preview checkout of {1}/{2}' format: { 
		self model repositoryName.
		self model targetName }
]
