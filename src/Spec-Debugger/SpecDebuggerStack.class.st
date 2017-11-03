"
A SpecDebuggerStack is responsible for displaying and managing the stack. All actions from the context menu of the stack are loaded by using the pragmas 'self contextMenuDebuggingActions'.
"
Class {
	#name : #SpecDebuggerStack,
	#superclass : #ComposablePresenter,
	#instVars : [
		'sessionHolder',
		'listWidget',
		'actionsCache'
	],
	#classVars : [
		'DoItFilterEnabled',
		'FilterDictionary',
		'KCFilterEnabled',
		'LinkSelectorFilterEnabled',
		'NilSelectorsFilterEnabled'
	],
	#category : #Spec-Debugger
}

{ #category : #accessing }
SpecDebuggerStack class >> defaultStackSize [

	^ 20
]

{ #category : #'filters creation' }
SpecDebuggerStack class >> doItFilter [
	^ SelectorFilter forSelector: #doIt
]

{ #category : #menu }
SpecDebuggerStack class >> dynamicStackMenu: aBuilder [
	<contextMenu>
	<specDebuggerStackMenuNew>
	
	| target |
	target := aBuilder model.
	target currentContext ifNil: [ ^ target ].
	target currentContext class refactoringMethod: aBuilder.
	target currentContext class methodRefactoring: aBuilder.
		
	aBuilder model stack generateContextMenuDebuggingActions do: [ :anAction | 
		anAction asMenuRegistrationIn: aBuilder ].
	aBuilder
]

{ #category : #accessing }
SpecDebuggerStack class >> enabledFilters [
	| enabledFilters |
	
	enabledFilters := OrderedCollection new.
	self filterDoItSelectors ifTrue: [ 
		enabledFilters addLast: self doItFilter ].
	self filterKernelClasses ifTrue: [ 
		enabledFilters addLast: self kernelClassesFilter ].
	self filterNilSelectors ifTrue: [ 
		enabledFilters addLast: self nilMsgsFilter ].
	self filterLinkSelectors ifTrue: [ 
	enabledFilters addLast: self linkMessagesFilter].	
	^ enabledFilters
 
]

{ #category : #'settings filters' }
SpecDebuggerStack class >> filterDoItSelectors [
	^ DoItFilterEnabled ifNil:[ DoItFilterEnabled := false ]
]

{ #category : #'settings filters' }
SpecDebuggerStack class >> filterDoItSelectors: aBoolean [
	DoItFilterEnabled := aBoolean
]

{ #category : #'settings filters' }
SpecDebuggerStack class >> filterKernelClasses [
	^ KCFilterEnabled ifNil:[ KCFilterEnabled := false ]
]

{ #category : #'settings filters' }
SpecDebuggerStack class >> filterKernelClasses: aBoolean [
	KCFilterEnabled := aBoolean
]

{ #category : #'settings filters' }
SpecDebuggerStack class >> filterLinkSelectors [
  	^ LinkSelectorFilterEnabled ifNil:[ LinkSelectorFilterEnabled := false ]
]

{ #category : #'settings filters' }
SpecDebuggerStack class >> filterLinkSelectors: aBoolean [
	LinkSelectorFilterEnabled := aBoolean
]

{ #category : #'settings filters' }
SpecDebuggerStack class >> filterNilSelectors [
  	^ NilSelectorsFilterEnabled ifNil:[ NilSelectorsFilterEnabled := false ]
]

{ #category : #'settings filters' }
SpecDebuggerStack class >> filterNilSelectors: aBoolean [
	NilSelectorsFilterEnabled := aBoolean
]

{ #category : #'class initialization' }
SpecDebuggerStack class >> initialize [

	FilterDictionary := Dictionary new.
]

{ #category : #'filters creation' }
SpecDebuggerStack class >> kernelClassesFilter [
	^ KernelClassesFilter new.
	
]

{ #category : #'filters creation' }
SpecDebuggerStack class >> linkMessagesFilter [ 
	^ SelectorFilter forSelectors: (OrderedCollection with: #ctxtEnsure: with: #valueInContext:)
]

{ #category : #'filters creation' }
SpecDebuggerStack class >> nilMsgsFilter [ 
	^ SelectorFilter forSelectors: (OrderedCollection with: #isNil with: #ifNotNil: with: #ifNil)
]

{ #category : #specs }
SpecDebuggerStack class >> spec [
	<spec: #default>
	
	^ SpecLayout composed
		add: #list 
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> browseClassRefs [

	self currentContext ifNotNil: [ :aContext || class|
		class := aContext methodClass theNonMetaClass.
		class isTrait 
			ifTrue: [ self systemNavigation browseAllUsersOfTrait: class ]
			ifFalse: [ self systemNavigation browseAllCallsOnClass: class ] ]

]

{ #category : #'actions browsing' }
SpecDebuggerStack >> browseClassVarRefs [
	"1/17/96 sw: devolve responsibility to the class, so that the code that does the real work can be shared"

	| cls |
	cls := self currentClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation  browseClassVarRefs: cls]
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> browseClassVariables [
	"Browse the class variables of the selected class."
	| cls |
	cls := self currentClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation  browseClassVariables: cls]

]

{ #category : #'actions browsing' }
SpecDebuggerStack >> browseInstVarRefs [
	"1/26/96 sw: real work moved to class, so it can be shared"
	| cls |
	cls := self currentClassOrMetaClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation browseInstVarRefs: cls]
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> browseMessages [
	"Present a menu of all messages sent by the current message. 
	Open a message set browser of all implementors of the message chosen."

	self currentMessageName ifNotNil: [   
		self systemNavigation browseAllImplementorsOf: self currentMessageName ]
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> browseMethodFull [
	"Create and schedule a full Browser and then select the current class and message."

	self currentClassOrMetaClass ifNotNil: [ 
		Smalltalk tools browser 
			openOnClass: self currentClassOrMetaClass
			selector: self currentMessageName ]
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> browseSendersOfMessages [
	"Present a menu of the current message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self currentMessageName ifNotNil: [   
		self systemNavigation browseAllSendersOf: self currentMessageName ]
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> browseVersions [
	"Create and schedule a message set browser on all versions of the 
	currently selected message selector."

	Smalltalk tools versionBrowser
		browseVersionsForClass:  self currentClassOrMetaClass 
		selector: self currentMessageName
]

{ #category : #printing }
SpecDebuggerStack >> columnsFor: anEntry [
	"protocol: printing"

	"Answer the array of contexts."

	| method classLocation receiverClass |
	method := anEntry home method.
	classLocation := method methodClass.
	receiverClass := anEntry receiver class.

	"If the receiver is differen from the implementor, show the original method implementor"
	classLocation := classLocation = receiverClass
		ifTrue: [ classLocation name ]
		ifFalse: [ receiverClass name , '(' , classLocation name , ')' ].
	anEntry isBlockContext
		ifTrue: [ classLocation := '[...] in ' , classLocation ].
	^ {classLocation.
	method selector asString}
]

{ #category : #initialization }
SpecDebuggerStack >> contextMenu: aMenu [
	"Create the context menu dynamically."
	
	self flag: 'Spec ignores the shortcuts set in the menu. Use another mechanism to load them'.
	"This method of constructing the menu is used as it allow one to reuse the MenuRegistration class for creating the actual menu morphs."							
	^ aMenu addAllFromPragma: 'specDebuggerStackMenuNew' target: self debugger.
]

{ #category : #'actions lookup' }
SpecDebuggerStack >> contextMenuDebuggingActionsPragmas [

	^ { #contextMenuDebuggingAction }
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> currentClass [

	^ self currentContext methodClass
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> currentClassOrMetaClass [

	^ self currentContext methodClass
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> currentContext [
	
	^ self debugger currentContext
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> currentMessageName [

	^ self currentContext methodSelector
]

{ #category : #private }
SpecDebuggerStack >> debugger [
	"Return the instance of the debugger containing this toolbar"
	
	^  owner
]

{ #category : #filtering }
SpecDebuggerStack >> enabledFilters [

	^ self class enabledFilters
]

{ #category : #actions }
SpecDebuggerStack >> expand [
	"If the last element of the visible stack is selected, expand the stack,
	if more elements are available."
	
	self list selectedItem ifNil: [ ^ self ]. 
	
	self list listItems ifNotNil: [ :aList |
		(self list selectedIndex = self list listSize and: [
			self list selectedItem sender notNil ]) ifTrue: [ 
				self expandBy: self class defaultStackSize ] ]
	
]

{ #category : #actions }
SpecDebuggerStack >> expandBy: anInteger [
	| selectionIndex |
	
	selectionIndex := self list selectedIndex.
	self list items: (self filteredStackOfSize: anInteger + self list listSize).
	self list setSelectedIndex: selectionIndex
	
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> fileOutMessage [
	"Put a description of the selected message on a file"

	self currentMessageName ifNotNil: [
		Cursor write showWhile:
			[ self currentClassOrMetaClass fileOutMethod: self currentMessageName ] ]
]

{ #category : #filtering }
SpecDebuggerStack >> filteredStackOfSize: anInteger [

	^ self debugger class filterCommonMessageSends 
		ifTrue: [
			self session 
				stackOfSize: anInteger 
				usingFilters: self enabledFilters ]
		ifFalse: [ 
			self session stackOfSize: anInteger ]
		
	 
]

{ #category : #'actions lookup' }
SpecDebuggerStack >> generateContextMenuDebuggingActions [

	actionsCache ifNotNil: [  ^ actionsCache ].
	"Cache the actions as this method is called multiple times: 
	two times when the menu is created and one time when the registering shortcuts."
	^ actionsCache := self session class 
		debuggingActionsForPragmas: self contextMenuDebuggingActionsPragmas 
		for: self debugger
]

{ #category : #initialization }
SpecDebuggerStack >> initialize [

	sessionHolder := nil asValueHolder.
	actionsCache := nil.
	super initialize.
	
	"self setDefaultFilters."
]

{ #category : #initialization }
SpecDebuggerStack >> initializePresenter [

	super initializePresenter.

	sessionHolder whenChangedDo: [ :aSession |
		self updateContentFromSession: aSession ] 

]

{ #category : #initialization }
SpecDebuggerStack >> initializeWidgets [

	listWidget := self newMultiColumnList.
	
	self list displayBlock: [ :aContext | self columnsFor: aContext ].
	self list menu: [ :aMenu |
		self contextMenu: aMenu]
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> inspectInstances [
	"Inspect all instances of the selected class."

	self currentClassOrMetaClass ifNotNil: [ :aClass |
		aClass theNonMetaClass inspectAllInstances ]
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> inspectSubInstances [
	"Inspect all instances of the selected class and all its subclasses"

	self currentClassOrMetaClass ifNotNil: [ :aClass |
		aClass theNonMetaClass inspectSubInstances ]
]

{ #category : #accessing }
SpecDebuggerStack >> list [

	^ listWidget
]

{ #category : #api }
SpecDebuggerStack >> listItems [

	^ self list listItems
]

{ #category : #'actions browsing' }
SpecDebuggerStack >> methodHierarchy [
	"Create and schedule a method browser on the hierarchy of implementors."

	self systemNavigation 
			methodHierarchyBrowserForClass: self currentClassOrMetaClass 
			selector: self currentMessageName

]

{ #category : #api }
SpecDebuggerStack >> selectedIndex [

	^ self list selectedIndex
]

{ #category : #api }
SpecDebuggerStack >> selectedItem [

	^ self list selectedItem
]

{ #category : #accessing }
SpecDebuggerStack >> session [

	^ sessionHolder value
]

{ #category : #accessing }
SpecDebuggerStack >> session: aSession [

	sessionHolder value: aSession
]

{ #category : #api }
SpecDebuggerStack >> setSelectedItem: anItem [

	self list setSelectedItem: anItem
]

{ #category : #api }
SpecDebuggerStack >> takeKeyboardFocus [

	self list takeKeyboardFocus
]

{ #category : #updating }
SpecDebuggerStack >> updateContentFromSession: aSession [
	
	self list items: (aSession 
		ifNil: [ OrderedCollection new ] 
		ifNotNil: [ self filteredStackOfSize: self class defaultStackSize ] )
]

{ #category : #updating }
SpecDebuggerStack >> updateDebuggingActions [

	actionsCache ifNotNil: [ 
		actionsCache do: [ :each |
			each keymap ifNotNil: [ :aShortcut |
				self list removeKeyCombination: aShortcut ] ] ].
	actionsCache := nil.
	self generateContextMenuDebuggingActions.
	
	actionsCache do: [ :each |
		each keymap ifNotNil: [ 
			self list bindKeyCombination: each keymap toAction:  [ each execute ] ] ].
]

{ #category : #updating }
SpecDebuggerStack >> updateForSelectionChanged [

	self
		expand;
		takeKeyboardFocus; "This is needed, as otherwise, the inspectors will take the focus"
		updateDebuggingActions 
]

{ #category : #api }
SpecDebuggerStack >> whenListChanged: aBlock [

	self list whenListChanged: aBlock
]

{ #category : #api }
SpecDebuggerStack >> whenSelectedItemChanged: aBlock [

	self list whenSelectedItemChanged: aBlock
]
