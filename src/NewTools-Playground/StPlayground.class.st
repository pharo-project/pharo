"
! The Playground
It is useful to explore ideas or learn something before writing it to a more definitive place.
In older versions of Pharo, this tool was known as Workspace.

The idea of a playground is not new in smalltalk systems and many other dynamic languages have implemented in one way or another some features of it, usually through a REPL, which shares much of a playground functionality. 

Some of the things a playground can do for you: 

- it can execute code (and print its results).
- it can debug some piece of code you just typed.
- it can search references, implementors and senders
- it can store variables (both locally or shared to all playgrounds).

!! Opening:
You usually will open a playground by choosing it from the Pharo menu (Playground option), but it can also be opened programatically. 
[[[language=Smalltalk
StPlayground open.
StPlayground openContents: 'Some scripting'
]]]

!! Using the playground
Most of the functionalities a playground does can be found in the context menu of it (==<meta+t>== or right button click), and we will not explain in detail each of them. But there are some usages that may requirse some more documentation than ""self discovery"". Here are some hints you may find useful: 

!!! Keyboard friendly
There is a shortcut, ==<meta+t>==, that will show the context menu corresponding to the playground. Additionaly, there will be entries to control also what you can find in the toolbar or status bar. This redundancy is intentional and it will allow the user to navigate through all the options of the playgrounds without needing a secondary device (which, of course, can be used anyway if desired).

!!! The print it popover
If you try to print results of a computation, you will see that you receive a popover with the results of the execution (e.g. ==21*2== will show a popover with the ==42== number). There are some other things you can do with this popover besides just dismiss it (pressing ==<esc>==, ==<backspace>== or just focusing somewhere else).
You can also: 

- press ==<cr>== to print the result into your playground (it will be shown enclosed with ""quotation marks"", as a comment.
- press ==<meta+i>== to inspect the result.

!!! The bindings window
The bindings window will be shown by selecting the ""Bindings"" option of the context menu or toolbar.
You will see the variables panel and an inspector allowing you to explore/modify the values of a variable. There is also a context menu in the variables list that will allow you some other options. One interesting option you will see is the ""make variable shared"" option. This option will make the variable common to all playgrounds. It is an easy way to explore some posibilities in a separated space, not modifying what you already have there.

See *class:StPlaygroundBindingsPresenter*

!!! The pages window
The pages window will be shown by selecting the ""Pages"" option of the context menu or toolbar.
This window will list all playgrounds pages stored in the playground cache directory. You can choose any playground edited in any moment and load it into your playgroud.

See *class:StPlaygroundPagesPresenter*

"
Class {
	#name : #StPlayground,
	#superclass : #StPresenter,
	#instVars : [
		'text',
		'statusBar',
		'bindings',
		'lineLabel',
		'toggleLineNumberButton',
		'page'
	],
	#classVars : [
		'CacheDirectory',
		'PreferredExtent'
	],
	#category : #'NewTools-Playground-View'
}

{ #category : #accessing }
StPlayground class >> cacheDirectory [
	
	^ CacheDirectory ifNil: [ self defaultCacheDirectory ]
]

{ #category : #accessing }
StPlayground class >> cacheDirectory: aDirectory [
	
	self flag: #TODO. "This go to settings"
	CacheDirectory := aDirectory ensureCreateDirectory
]

{ #category : #private }
StPlayground class >> defaultCacheDirectory [
	
	^ (FileLocator localDirectory / 'play-cache') ensureCreateDirectory
]

{ #category : #accessing }
StPlayground class >> defaultExtent [

	^ 600@400
]

{ #category : #accessing }
StPlayground class >> defaultShowLineNumbers [

	^ true
]

{ #category : #specs }
StPlayground class >> defaultSpec [

	^ SpBoxLayout newVertical
		add: #text;
		yourself
]

{ #category : #accessing }
StPlayground class >> defaultTitle [ 

	^ 'Playground'
]

{ #category : #'class initialization' }
StPlayground class >> initialize [

	"Make playground active by default"
	self register
]

{ #category : #'instance creation' }
StPlayground class >> open [ 
	<script>
	
	^ self new openWithSpec
]

{ #category : #'instance creation' }
StPlayground class >> openContents: aString [
	"Open a new workspace with the given contents. It looks better when aString is code since the style is applied"

	^ self new
		contents: aString;
		openWithSpec
]

{ #category : #private }
StPlayground class >> preferredExtent [

	^ PreferredExtent ifNil: [ PreferredExtent := self defaultExtent ]
]

{ #category : #private }
StPlayground class >> preferredExtent: aPoint [

	PreferredExtent := aPoint
]

{ #category : #'tools registry' }
StPlayground class >> register [
	"Add ourselves to registry. See [Smalltalk tools]" 
	<script>

	self registerToolsOn: Smalltalk tools
]

{ #category : #'tools registry' }
StPlayground class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]" 

	registry register: self as: #workspace

]

{ #category : #'private builder' }
StPlayground >> buildMenu [
	| menuSpec |
	
	menuSpec := (PragmaMenuBuilder 
		pragmaKeyword: WorldState discoveredMenuPragmaKeyword 
		model: self)
		menuSpec.
	^ menuSpec asMenuBarPresenter
	
]

{ #category : #'private builder' }
StPlayground >> buildStatusBar [ 
	| newStatusBar |

	(newStatusBar := SpPresenter new)
		layout: (SpBoxLayout newHorizontal
			borderWidth: 3;
			add: (lineLabel := newStatusBar newLabel) expand: false;
			addLast: (toggleLineNumberButton := newStatusBar newToggleButton) expand: false;
			yourself).

	lineLabel addStyle: 'dim'.
	self updateLineNumber.
	toggleLineNumberButton
		label: (StShowLineNumbersCommand iconLabelFor: self showLineNumbers);
		help: StShowLineNumbersCommand defaultDescription;
		state: self showLineNumbers;
		whenChangedDo: [ :state | self showLineNumbers: state ].

	^ newStatusBar
]

{ #category : #'private builder' }
StPlayground >> buildToolbar [

	^ self toolbarActions 
		asToolbarPresenterWith: [ :presenter | 
			presenter 
				displayMode: self application toolbarDisplayMode;
				addStyle: 'stToolbar' ]
]

{ #category : #accessing }
StPlayground >> contents: aString [

	text text: aString
]

{ #category : #'api-focus' }
StPlayground >> defaultKeyboardFocus [

	^ text
]

{ #category : #initialization }
StPlayground >> initialize [

	page := StPlaygroundPage new.
	super initialize
]

{ #category : #initialization }
StPlayground >> initializePresenters [
	
	text := self newCode
		interactionModel: StPlaygroundInteractionModel new;
		lineNumbers: self showLineNumbers;
		contextMenu: [ self menuActions asMenuPresenter ];
		contextKeyBindings: self menuActions asKMCategory;
		whenTextChangedDo: [ :aString | page contents: aString ];
		yourself.
		
	text eventHandler 
		whenKeyUpDo: [ :event | self updateLineNumber ];
		whenMouseUpDo: [ :event | self updateLineNumber ];
		"before taking focus position will be nil, ensure I have the correct one"
		whenFocusReceivedDo: [ self updateLineNumber ].
		
	self updatePresenter
]

{ #category : #initialization }
StPlayground >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter.
	aWindowPresenter 
		title: self class defaultTitle;
		initialExtent: self class preferredExtent;
		windowIcon: self windowIcon;
		toolbar: self buildToolbar;
		statusBar: (statusBar := self buildStatusBar);
		whenResizingDo: [ :ann | self preferredExtent: ann newSize ];
		whenOpenedDo: [ text takeKeyboardFocus ]

]

{ #category : #accessing }
StPlayground >> interactionModel [

	^ text interactionModel
]

{ #category : #actions }
StPlayground >> loadPage: aFileReference [

	page := StPlaygroundPage fromReference: aFileReference.
	text text: page contents
]

{ #category : #private }
StPlayground >> menuActions [

	^ CmCommandGroup forSpec
		beRoot;
		register: ((CmCommandGroup named: 'Playground') asSpecGroup
			register: ((CmCommandGroup named: 'Base options') asSpecGroup
				beDisplayedAsGroup;
				register: (StPlaygroundPublishCommand forSpecContext: self);
				register: (StPlaygroundBindingsCommand forSpecContext: self);
				register: (StPlaygroundPagesCommand forSpecContext: self);
				yourself);
			register: ((CmCommandGroup named: 'Miscelaneous options') asSpecGroup
				beDisplayedAsGroup;
				register: (StShowLineNumbersCommand forSpecContext: self);
				yourself);
			yourself);
		yourself
]

{ #category : #accessing }
StPlayground >> page [

	^ page
]

{ #category : #private }
StPlayground >> preferredExtent: aSize [

	(self window isMaximized 
		or: [ self window isMinimized ])
		ifTrue: [ ^ self ].
	self class preferredExtent: aSize
]

{ #category : #accessing }
StPlayground >> selectAll [
	
	^ text selectAll
]

{ #category : #accessing }
StPlayground >> selectedTextOrLine [
	
	^ text selectedTextOrLine
]

{ #category : #private }
StPlayground >> showLineNumbers [

	^ StPharoSettings codeShowLineNumbers
]

{ #category : #'private actions' }
StPlayground >> showLineNumbers: aBoolean [

	StPharoSettings codeShowLineNumbers: aBoolean.
	text lineNumbers: aBoolean.
	toggleLineNumberButton label: (StShowLineNumbersCommand iconLabelFor: self showLineNumbers)
]

{ #category : #actions }
StPlayground >> toggleShowLineNumbers [

	self showLineNumbers: self showLineNumbers not
]

{ #category : #'private builder' }
StPlayground >> toolbarActions [

	^ CmCommandGroup forSpec
		register: (CmCommandGroup forSpec
			register: (StPlaygroundDoItCommand forSpecContext: self);
			register: (StPlaygroundPublishCommand forSpecContext: self);
			register: (StPlaygroundBindingsCommand forSpecContext: self);
			register: (StPlaygroundPagesCommand forSpecContext: self);
			yourself);
		yourself
]

{ #category : #'private updating' }
StPlayground >> updateLineNumber [
	| pos |
	
	lineLabel ifNil: [ ^ self ].	
	pos := text cursorPosition.
	lineLabel label: (pos 
		ifNotNil: [ ('Line: {1}:{2}' format: { pos y. pos x }) ]
		ifNil: [ '' ])
]

{ #category : #'private updating' }
StPlayground >> updatePresenter [

	self updateLineNumber
]

{ #category : #initialization }
StPlayground >> windowIcon [
	
	^ self application iconNamed: #workspace
]
