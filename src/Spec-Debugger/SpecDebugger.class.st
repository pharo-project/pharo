"
A SpecDebugger is a UI build in Spec that provides Smalltalk debugging functionality by using a DebugSession.

All the debugging logic is in DebugSession and the corresponding debugging actions. SpecDebugger only provides a graphical interface to access that functionality. The debugging actions are managed by the toolbar widget; the stack and all its related actions are managed by the stack widget.

"
Class {
	#name : #SpecDebugger,
	#superclass : #ComposablePresenter,
	#instVars : [
		'sessionHolder',
		'labelHolder',
		'stack',
		'code',
		'toolbar',
		'contextInspector',
		'receiverInspector',
		'browsedEnvironment'
	],
	#classVars : [
		'AlwaysOpenFullDebugger',
		'ErrorRecursion',
		'FilterCommonMessageSends',
		'LogDebuggerStackToFile'
	],
	#category : 'Spec-Debugger'
}

{ #category : #settings }
SpecDebugger class >> alwaysOpenFullDebugger [
	^ AlwaysOpenFullDebugger ifNil: [AlwaysOpenFullDebugger := false]
]

{ #category : #settings }
SpecDebugger class >> alwaysOpenFullDebugger: aBoolean [
	AlwaysOpenFullDebugger := aBoolean.
]

{ #category : #utilities }
SpecDebugger class >> closeAllDebuggers [
	"SpecDebugger closeAllDebuggers"
	(MorphicWindowAdapter allInstances select: [:w | 
		(w model isKindOf: WindowPresenter) and: [ 
			w model model isKindOf: self ]  ])
			do: [:w | w delete ].
			
	SpecPreDebugWindow allInstances do: #delete.
			
		
			
	
]

{ #category : #accessing }
SpecDebugger class >> defaultTitle [

	^ 'Spec Debugger'
]

{ #category : #'menu code' }
SpecDebugger class >> dynamicCodeContextMenu: aBuilder [
	<contextMenu>
	<specDebuggerCodeContextMenu>

	aBuilder model codeContextMenuDebuggingActions do: [ :anAction | 
		anAction asMenuRegistrationIn: aBuilder ]
]

{ #category : #settings }
SpecDebugger class >> filterCommonMessageSends [
	^ FilterCommonMessageSends ifNil: [FilterCommonMessageSends := false]
]

{ #category : #settings }
SpecDebugger class >> filterCommonMessageSends: aBoolean [
	FilterCommonMessageSends := aBoolean
]

{ #category : #'class initialization' }
SpecDebugger class >> initialize [ 
	
	ErrorRecursion := false.

	"Ungly check to make sure the debugger will work until everything is in the image"
	(Smalltalk tools respondsTo: #registerDebugger:withRank:) ifFalse: [ ^ self ].

	"Add ourselves to registry. See [Smalltalk tools]" 
	Smalltalk tools registerDebugger: self withRank: 2.
]

{ #category : #settings }
SpecDebugger class >> logDebuggerStackToFile [
	^ LogDebuggerStackToFile ifNil: [LogDebuggerStackToFile := true]
]

{ #category : #settings }
SpecDebugger class >> logDebuggerStackToFile: aBoolean [
	 LogDebuggerStackToFile := aBoolean
]

{ #category : #'instance creation' }
SpecDebugger class >> on: aDebugSession [

	^ self new 	
			session: aDebugSession
]

{ #category : #opening }
SpecDebugger class >> openOn: aDebugSession withFullView: aBool [
	
	self openOn: aDebugSession withFullView: aBool andNotification: nil
]

{ #category : #opening }
SpecDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	
	| debugger |
	debugger := self on: aDebugSession.
	
	aBool | self alwaysOpenFullDebugger
		ifTrue: [ debugger openWithFullView ]
		ifFalse: [ debugger openWithNotification: aString]
]

{ #category : #'tools registry' }
SpecDebugger class >> register [

	self registerToolsOn: Smalltalk tools
]

{ #category : #'tools registry' }
SpecDebugger class >> registerToolsOn: registry [

	"Add ourselves to registry. See [Smalltalk tools]" 
	registry register: self as: #debugger

]

{ #category : #accessing }
SpecDebugger class >> sessionClass [

	^ DebugSession 
]

{ #category : #specs }
SpecDebugger class >> spec [
	<spec>
	
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #stack;
				add: #toolbar height: 25;
				addSplitter;
				add: #code;
				addSplitter;
				newRow: [ :row | 
					row
						add: #receiverInspector;
						add: #contextInspector ] ]
]

{ #category : #specs }
SpecDebugger class >> specOptimized [
	<spec: #default>
	| toolbarHeight |
	
	toolbarHeight := self toolbarHeight.
	
	^ SpecLayout composed
		add: #stack origin: 0@0 corner: 1@0.25;
		add: #toolbar origin: 0@0.25 corner: 1@0.25 offsetOrigin: 0@0 offsetCorner: 0@toolbarHeight;
		addHSplitter;
		add: #code origin: 0@0.25 corner: 1@0.80 offsetOrigin: 0@toolbarHeight offsetCorner: 0@0;
		addHSplitter;
		newRow: [ :r |
			r 
				add: #receiverInspector;
				addSplitter;
				add: #contextInspector]
		top: 0.8;
		yourself 
]

{ #category : #accessing }
SpecDebugger class >> stackWidgetClass [

	^ SpecDebuggerStack 
]

{ #category : #icons }
SpecDebugger class >> taskbarIconName [

	^#smallDebugIcon
]

{ #category : #accessing }
SpecDebugger >> browsedEnvironment [
	^ browsedEnvironment ifNil: [ browsedEnvironment := RBBrowserEnvironment new ]
]

{ #category : #actions }
SpecDebugger >> clear [
	
	self removeActionsForSession: self session.
	"No more code should follow after this, as the session might terminate this process."
	self session ifNotNil: [ :aSession | aSession terminate]
]

{ #category : #actions }
SpecDebugger >> close [
	
	self window delete
]

{ #category : #'accessing widgets' }
SpecDebugger >> code [

	^ code
]

{ #category : #'actions code' }
SpecDebugger >> codeContextMenuDebuggingActions [

	^ self session class 
		debuggingActionsForPragmas: #(contextMenuCodeDebuggingAction) 
		for: self
]

{ #category : #private }
SpecDebugger >> codeFont [ 
	^ StandardFonts codeFont
]

{ #category : #'accessing context' }
SpecDebugger >> context [
 
	^ self interruptedContext
]

{ #category : #'accessing widgets' }
SpecDebugger >> contextInspector [

	^ contextInspector
]

{ #category : #'accessing context' }
SpecDebugger >> currentContext [
	"I answer the context on which an action should be executed.
	This is either the selected context or if nothing is selected the interrupted context."
	
	^ self selectedContext isNil 
		ifTrue: [ self interruptedContext ]
		ifFalse: [ self selectedContext ]
]

{ #category : #api }
SpecDebugger >> initialExtent [ 
	"This code was taken from the old debugger to get the same behavior. Fell free to refactor"

	RealEstateAgent standardWindowExtent y < 400 "a tiny screen" 
		ifTrue: [^ 700@500].
	
	^ [ | w h |
		w := Display width.
		h := Display height.
		(w - (w // 3)) @ (h - (h // 5)) ]
		on: Error
		do: [ 850@600 ]
]

{ #category : #initialization }
SpecDebugger >> initialize [

	sessionHolder := nil asValueHolder.
	labelHolder := nil asValueHolder.
	super initialize.
	
	self whenWindowChanged: [ :w | 
		w whenClosedDo: [ self clear ] ].
	
	sessionHolder whenChangedDo: [:newSession :oldSession |
		self removeActionsForSession: oldSession.
		self registerActionsForSession: newSession.
		self updateToolbarFromSession: newSession.
		self updateStackFromSession: newSession ].
	
	labelHolder whenChangedDo: [ 
		self updateTitle ]
]

{ #category : #initialization }
SpecDebugger >> initializePresenter [
	super initializePresenter.
	self flag: 'some of this logic could be moved to the stack widget'.
	self flag: 'The toolbar should not be updated when the list changes, but when an action is perormed.'.
	self stack whenListChanged: [ :aList |
		aList isEmpty ifFalse: [ self stack setSelectedItem: aList first ].
		"Updating the toolbar will result in changing the button widgets.
		If the widget hasn't been opened, there will be no spec, which leads to an error."
		self spec ifNotNil: [
			self updateToolbar ] ].

	self stack whenSelectedItemChanged: [:aContext |
		self updateCodeFromContext: aContext.
		self updateInspectorsFromContext: aContext.
		self stack updateForSelectionChanged ].
]

{ #category : #initialization }
SpecDebugger >> initializeWidgets [
	stack := self instantiate: SpecDebuggerStack.
	toolbar := self instantiate: SpecDebuggerDynamicToolbar.
	code := self newCode.
	receiverInspector := self instantiate: InspectorNavigator.
	contextInspector := self instantiate: EyeDebuggerContextInspector.
	self code
		acceptBlock:
			[ :text :notifyer | self recompileMethodTo: text string inContext: self stack selectedItem notifying: notifyer ].
	self code whenBuiltDo: [ :ann | ann widget font: self codeFont ].
	self setFocusOrder
]

{ #category : #'accessing context' }
SpecDebugger >> interruptedContext [

	^ self session interruptedContext
]

{ #category : #accessing }
SpecDebugger >> interruptedProcess [

	^ self session interruptedProcess
]

{ #category : #accessing }
SpecDebugger >> label [

	^ labelHolder value
]

{ #category : #accessing }
SpecDebugger >> label: aString [

	labelHolder value: aString
]

{ #category : #api }
SpecDebugger >> okToChange [
	self code canDiscardEdits
		ifTrue: [ ^ true ].
	self code changed: #wantToChange.
	^ self code canDiscardEdits
]

{ #category : #opening }
SpecDebugger >> openWithFullView [
	"Create and schedule a full debugger with the given label. Do not
	terminate the current active process."
	self 
		openWithSpec;
		updateToolbar;
		updateCodeFromContext
]

{ #category : #opening }
SpecDebugger >> openWithNotification: notificationString [
	"Create and schedule a notifier view with the given label and message.
	A notifier view shows just the message or the first several lines of the
	stack, with a menu that allows the user to open a full debugger if so
	desired. "

	"NOTE: When this method returns, a new process has been scheduled to
	run the windows, and thus this notifier, but the previous active porcess
	has not been suspended. The sender will do this."
	
	SpecPreDebugWindow new 
		setTitle: self label;
		message: notificationString;
		openWithSpec;
		debugger: self
]

{ #category : #'accessing widgets' }
SpecDebugger >> receiverInspector [

	^ receiverInspector
]

{ #category : #actions }
SpecDebugger >> recompileMethodTo: aString inContext: aContext notifying: aNotifyer [

	^ aContext 
		ifNil: [ false ]
		ifNotNil: [
			self session recompileMethodTo: aString inContext: aContext notifying: aNotifyer ]
]

{ #category : #accessing }
SpecDebugger >> refactor [
	^ NautilusRefactoring model: self
]

{ #category : #initialization }
SpecDebugger >> registerActionsForSession: aSession [

	aSession ifNotNil: [
		aSession
			when: #restart send: #updateRestart to: self;
			when: #resume send: #updateResume to: self;
			when: #stepInto send: #updateStepInto to: self;
			when: #stepOver send: #updateStepOver to: self;
			when: #stepThrough send: #updateStepThrough to: self;
			when: #contextChanged send: #updateContextChanged to: self  ]
]

{ #category : #initialization }
SpecDebugger >> removeActionsForSession: aSession [

	aSession ifNotNil: [ 
		aSession  
			removeActionsForEvent: #restart;
			removeActionsForEvent: #resume;
			removeActionsForEvent: #stepInto;
			removeActionsForEvent: #stepOver;
			removeActionsForEvent: #stepThrough;
			removeActionsForEvent:#contextChanged ]. 
]

{ #category : #'accessing context' }
SpecDebugger >> selectTopContext [
	stack setSelectedItem: stack list listItems first
]

{ #category : #accessing }
SpecDebugger >> selectedClass [
	^ self currentContext class
]

{ #category : #'accessing context' }
SpecDebugger >> selectedContext [	

	^ self stack selectedItem
]

{ #category : #accessing }
SpecDebugger >> selectedMessage [
	^ self selectedMethod
]

{ #category : #accessing }
SpecDebugger >> selectedMethod [
	^ self currentContext method
]

{ #category : #accessing }
SpecDebugger >> selectedMethods [
	^ self stack selectedItem asOrderedCollection 
]

{ #category : #'old - public' }
SpecDebugger >> send [
	
	self okToChange ifTrue: [  
		self session stepInto: self currentContext ]

]

{ #category : #accessing }
SpecDebugger >> session [

	^ sessionHolder value
]

{ #category : #accessing }
SpecDebugger >> session: aSession [

	sessionHolder value: aSession.
	self label: aSession name asString
]

{ #category : #initialization }
SpecDebugger >> setFocusOrder [

	self focusOrder 
		removeAll;
		add: stack;
		add: toolbar;
		add: code;
		add: receiverInspector;
		add: contextInspector
]

{ #category : #'accessing widgets' }
SpecDebugger >> stack [

	^ stack
]

{ #category : #api }
SpecDebugger >> title [

	^ ((self session isContextPostMortem: self interruptedContext)  
			ifTrue: [ 'PostMortem: ' ]
			ifFalse: [ '' ]), (self label ifNil: [ '' ])
]

{ #category : #'accessing widgets' }
SpecDebugger >> toolbar [

	^ toolbar
]

{ #category : #'updating widgets' }
SpecDebugger >> updateCodeFromContext [
self updateCodeFromContext:self currentContext
]

{ #category : #'updating widgets' }
SpecDebugger >> updateCodeFromContext: aContext [ 

	aContext 
		ifNil: [
			self code 
				aboutToStyle: false;
				text: '';
				behavior: nil;
				setSelection: (1 to: 0) ]
		ifNotNil: [ |selectionInterval|
			selectionInterval := self session selectedCodeRangeForContext: aContext.
			self code
				aboutToStyle: true;
				text: aContext sourceCode;
				doItReceiver: aContext home receiver;
				doItContext: aContext;
				behavior: aContext home receiver class;
				setSelection: selectionInterval ]
]

{ #category : #'updating actions' }
SpecDebugger >> updateContextChanged [
	|selection|
	
	selection := self stack selectedItem.
	self updateStep.
	self stack setSelectedItem: selection
]

{ #category : #updating }
SpecDebugger >> updateContextInspectorFromContext: aContext [
	| oldSelection newSelectionIndex   |
	
	oldSelection := contextInspector list selectedItem.
	
	contextInspector inspect: aContext.
	
	newSelectionIndex := 1.
	oldSelection ifNotNil: 
	[	contextInspector list listItems withIndexDo: 
			[ :item :index | item label = oldSelection label ifTrue: [ newSelectionIndex := index ] ].
	].
	contextInspector list setSelectedIndex: newSelectionIndex.

]

{ #category : #updating }
SpecDebugger >> updateInspectorsFromContext: aContext [
	self updateContextInspectorFromContext: aContext.
	self updateReceiverInspectorFromContext: aContext.
]

{ #category : #updating }
SpecDebugger >> updateReceiverInspectorFromContext: aContext [
	| receiver oldSelection newSelectionIndex   |

	oldSelection := receiverInspector inspector ifNotNil: [ :inspector | inspector list selectedItem ].

	receiver := aContext ifNotNil: [ aContext home receiver ]. 
	receiverInspector inspect: receiver.	
		
	newSelectionIndex := 1.
	oldSelection ifNotNil: 
	[	receiverInspector inspector list listItems withIndexDo: 
			[ :item :index | item label = oldSelection label ifTrue: [ newSelectionIndex := index ] ].
	].
	receiverInspector inspector list setSelectedIndex: newSelectionIndex.
	

]

{ #category : #'updating actions' }
SpecDebugger >> updateRestart [

	self updateStep 
	
]

{ #category : #'updating actions' }
SpecDebugger >> updateResume [

	
]

{ #category : #'updating widgets' }
SpecDebugger >> updateStackFromSession: aSession [
	
	self stack session: aSession.
]

{ #category : #'updating actions' }
SpecDebugger >> updateStep [

	self updateStackFromSession: self session.
	

]

{ #category : #'updating actions' }
SpecDebugger >> updateStepInto [

	self updateStep 
]

{ #category : #'updating actions' }
SpecDebugger >> updateStepOver [

	self updateStep 
	
]

{ #category : #'updating actions' }
SpecDebugger >> updateStepThrough [

	self updateStep 
]

{ #category : #'updating widgets' }
SpecDebugger >> updateToolbar [
	
	self toolbar update
]

{ #category : #'updating widgets' }
SpecDebugger >> updateToolbarFromSession: aSession [

	self toolbar session: aSession.
]
