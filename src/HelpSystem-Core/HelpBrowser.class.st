"
A HelpBrowser is used to display a hierarchy of help topics and their contents.

Instance Variables
	rootTopic: 		<HelpTopic>
	window:			<StandardWindow>	 
	treeMorph:		<PluggableTreeMorph>
	contentMorph:		<Morph>			

rootTopic
	- xxxxx

window
	- xxxxx

treeMorph 
	- xxxxx

contentMorph 
	- xxxxx


"
Class {
	#name : #HelpBrowser,
	#superclass : #Model,
	#instVars : [
		'rootTopic',
		'window',
		'treeMorph',
		'helpTopicCache',
		'sourceTextModel'
	],
	#classVars : [
		'DefaultHelpBrowser'
	],
	#category : #'HelpSystem-Core-UI'
}

{ #category : #accessing }
HelpBrowser class >> defaultHelpBrowser [
	^ DefaultHelpBrowser ifNil: [DefaultHelpBrowser := self]
	
]

{ #category : #accessing }
HelpBrowser class >> defaultHelpBrowser: aClass [
	"Use a new help browser implementation"
	
	DefaultHelpBrowser := aClass
	
]

{ #category : #'world menu' }
HelpBrowser class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Help Browser')
		parent: #Help;
		order: 1.0;
		action: [ self open ];
		iconName: #smallHelpIcon;
		help: 'Explore a list of help topics to learn to use Pharo tools.';
		withSeparatorAfter 
]

{ #category : #ui }
HelpBrowser class >> menuOn: aBuilder [
	<HelpBrowserContextMenu>
	
	(aBuilder item: #'Do it' translated)
		keyText: 'd';
		selector: #doIt;
		help: nil;
		iconName: #smallDoIt.
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		help: nil;
		iconName: #smallPrintIt.
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		help: nil;
		iconName: #smallInspectIt.

]

{ #category : #'instance creation' }
HelpBrowser class >> open [
	^self openOn: SystemHelp
]

{ #category : #'instance creation' }
HelpBrowser class >> openOn: aHelpTopic [
	"Open the receiver on the given help topic or any other object that can be transformed into
	 a help topic by sending #asHelpTopic."
	
	^(self defaultHelpBrowser new)
		rootTopic: aHelpTopic;
		open;
		yourself
]

{ #category : #icons }
HelpBrowser class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^ #bookIcon
]

{ #category : #accessing }
HelpBrowser class >> theme [
	^ Smalltalk ui theme 
]

{ #category : #ui }
HelpBrowser >> close [
	window notNil ifTrue: [window delete]
]

{ #category : #accessing }
HelpBrowser >> doItContext [
	^ nil
]

{ #category : #accessing }
HelpBrowser >> doItReceiver [
	^ nil
]

{ #category : #actions }
HelpBrowser >> helpTopic [
	^ helpTopicCache ifNil: [ helpTopicCache := rootTopic asHelpTopic ]
]

{ #category : #ui }
HelpBrowser >> initWindow [
	| toolbar dock contentMorph | 
	window := StandardWindow new.
	window model: self.
	window title: 'Help Browser'.
	toolbar := window
		newToolbar:
			{(window
				newButtonFor: self
				getState: nil
				action: #refresh
				arguments: nil
				getEnabled: nil
				labelForm: (self iconNamed: #refreshIcon)
				help: 'Refresh' translated)}.
	dock := window newToolDockingBar.
	dock addMorphBack: toolbar.
	window
		addMorph: dock
		fullFrame:
			(LayoutFrame identity
				bottomFraction: 0;
				bottomOffset: dock minExtent y).	"Tree"
	treeMorph := PluggableTreeMorph new.
	treeMorph
		model: self;
		setSelectedSelector: #onItemClicked:.
	window
		addMorph: treeMorph
		fullFrame:
			(LayoutFrame identity
				rightFraction: 0.3;
				topOffset: dock minExtent y).	"Text"
	contentMorph := self sourceTextModel newScrolledText
		beForSmalltalkScriptingWithDefaultFont;
		beWrapped;
		yourself.
	contentMorph menuProvider: self selector: #shoutMorphFillMenu:.
	window
		addMorph: contentMorph
		fullFrame:
			(LayoutFrame identity
				leftFraction: 0.3;
				topOffset: dock minExtent y)
]

{ #category : #initialization }
HelpBrowser >> initialize [ 
	super initialize. 
	self initWindow.
 
]

{ #category : #ui }
HelpBrowser >> menu [
	^ (PragmaMenuBuilder pragmaKeyword: #HelpBrowserContextMenu model: self) menu.

]

{ #category : #actions }
HelpBrowser >> next [
	treeMorph setSelectionIndex: treeMorph getSelectionIndex + 1
]

{ #category : #events }
HelpBrowser >> onItemClicked: anItem [
	anItem ifNil: [ ^ self sourceTextModel setText: rootTopic asHelpTopic contents ].
	self sourceTextModel setText: anItem contents
]

{ #category : #ui }
HelpBrowser >> open [	
 	"Open the receivers window" 

	self refresh.	
	window openInWorld.
	 
]

{ #category : #actions }
HelpBrowser >> previous [
	treeMorph setSelectionIndex: (treeMorph getSelectionIndex - 1 max: 1)
]

{ #category : #actions }
HelpBrowser >> refresh [
	| helpTopic items |
	helpTopic := self helpTopic.
	window setLabel: helpTopic title.
	items := helpTopic subtopics collect: [ :each | HelpTopicListItemWrapper with: each ].
	treeMorph list: items.
	self sourceTextModel setText: helpTopic contents
]

{ #category : #accessing }
HelpBrowser >> rootTopic [
	^rootTopic
]

{ #category : #accessing }
HelpBrowser >> rootTopic: aHelpTopic [

	rootTopic := aHelpTopic. 
	self refresh 
]

{ #category : #actions }
HelpBrowser >> selectARootTopic: aHelpClass [
	"Used to unfold a particular topic"
	
	| item |
	item := treeMorph allItems detect: [:each | 
            each complexContents item key = aHelpClass name ].
   treeMorph selection: item
]

{ #category : #actions }
HelpBrowser >> selectTopicSatisfying: aBlock [
	"I select the first topic that satisfies the given block.
	If the topic has subtopics it is expanded. If the topic is not visible 
	the entire path to that topic is expanded."
	
	(self helpTopic pathToTopicSatisfying: aBlock) ifNotEmpty: [ :aCollection |
		treeMorph 
			selectPath: aCollection allButFirst
			in: (treeMorph scroller submorphs at: 1 ifAbsent: [^treeMorph]) ]
]

{ #category : #accessing }
HelpBrowser >> selectedClassOrMetaClass [
	^ nil
]

{ #category : #ui }
HelpBrowser >> shoutMorphFillMenu: aMenu [
	^ aMenu addAllFrom: self menu
]

{ #category : #accessing }
HelpBrowser >> sourceTextModel [
	^ sourceTextModel ifNil: [ sourceTextModel := RubScrolledTextModel new interactionModel: self ]
]

{ #category : #accessing }
HelpBrowser >> window [

	^ window
]
