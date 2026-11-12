"
I am the root of the Calypso commands. I am a Commander2 command, and I keep the helper methods that the v1 activators and the Calypso morphs (toolbar, tab, table cells, context menus) still call on a command, so that commands can move off the Commander v1 base classes.
"
Class {
	#name : 'ClyCommand',
	#superclass : 'CmCommand',
	#instVars : [
		'dropContext',
		'prepared'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'testing' }
ClyCommand class >> canBeExecutedInContext: aToolContext [
	^self isAbstract not
]

{ #category : 'accessing' }
ClyCommand class >> iconName [
	"For compatibility the actual icon named can be defined on class side.
	But right way it to use same instance side method.

	Not really nice to return nil. But it is already expected by many tools.
	So it is easy to allow nil.
	But users should use instance side method defaultMenuIcon which should return Form instance"
	^nil
]

{ #category : 'testing' }
ClyCommand class >> isAbstract [
	^self = ClyCommand
]

{ #category : 'testing' }
ClyCommand >> acceptVisitor: aVisitor [

	^ aVisitor visitCommand: self
]

{ #category : 'ui requests' }
ClyCommand >> application [

	^ StPharoApplication current
]

{ #category : 'execution' }
ClyCommand >> applyCommandResult [
	^ self
]

{ #category : 'testing' }
ClyCommand >> canBeExecuted [

	^ (self class canBeExecutedInContext: self context)
		and: [ self isAvailableInMenuOf: self context ]
]

{ #category : 'ui requests' }
ClyCommand >> confirm: aString [

	^ self application confirm:  aString
]

{ #category : 'toolbar' }
ClyCommand >> decoratesToolbarItem [
	"Whether the toolbar item of this command gets its balloon text and icon."
	^ true
]

{ #category : 'accessing' }
ClyCommand >> defaultMenuIcon [

	^ self defaultMenuIconFormSet ifNotNil: [ :formSet | formSet asForm ]
]

{ #category : 'accessing' }
ClyCommand >> defaultMenuIconFormSet [
	"By default the result is nil which is not really nice
	but it is already expected by many tools.	So it is easy to allow nil.
	For simple cases users should implement class side method #iconName"
	^self iconName ifNotNil: [ :iconName |
		Smalltalk ui iconFormSetNamed: iconName ]
]

{ #category : 'accessing' }
ClyCommand >> description [
	"By default the result is nil which is not really nice
	but it is already expected by many tools. So it is easy to allow nil"
	^nil
]

{ #category : 'accessing' }
ClyCommand >> dropContext [
	^ dropContext
]

{ #category : 'accessing' }
ClyCommand >> dropContext: aToolContext [
	dropContext := aToolContext
]

{ #category : 'execution' }
ClyCommand >> execute [

	(prepared or: [ self context isNil ])
		ifTrue: [ self executeRefactoring ]
		ifFalse: [
				[
					prepared := true.
					self prepareFullExecution.
					self isComplexRefactoring
						ifTrue: [ self executeComplexRefactoring ]
						ifFalse: [
								self executeRefactoring.
								self applyCommandResult ] ]
					on: Exception
					do: [ :exception | self processCommandFailure: exception ] ]
]

{ #category : 'execution' }
ClyCommand >> executeComplexRefactoring [

	"Same preview as the v1 path: apply the picked changes on OK, nothing on cancel."
	| submitDialog |
	submitDialog := SycRefactoringPreviewPresenter for: self scopes: self context refactoringScopes.
	submitDialog existChanges
		ifTrue: [
			submitDialog openDialog
				okAction: [
					submitDialog accept.
					self applyCommandResult ];
				cancelAction: [ :p | p beCancel ] ]
		ifFalse: [ submitDialog alert: 'There is no change produced by this refactoring' ].
	CmCommandAborted signal
]

{ #category : 'accessing' }
ClyCommand >> fullBrowserOrder [

	^ self order
]

{ #category : 'accessing' }
ClyCommand >> icon [

	^ self defaultMenuIcon
]

{ #category : 'accessing' }
ClyCommand >> iconName [
	"For compatibility it delegates actual logic to class side. But it should be just
		^nil
	Not really nice to return nil. But it is already expected by many tools.
	So it is easy to allow nil.
	When users want to define real icon instead of standard named icon
	they should override another instance side method #defaultMenuIcon
	which should return desired Form instance"
	^self class iconName
]

{ #category : 'initialization' }
ClyCommand >> initialize [
	super initialize.
	prepared := false
]

{ #category : 'testing' }
ClyCommand >> isAvailableInMenuOf: aToolContext [

	^ true
]

{ #category : 'menu' }
ClyCommand >> menuAlternatives [
	"The commands this command stands for in a context menu, one item each (the scope switcher). Empty when it is a single item."
	^ #()
]

{ #category : 'accessing' }
ClyCommand >> menuGroup [
	"The menu group of the command when ClyCommandGroupBuilder places it as a pure command (#buildMenuOf:context:pureRoots:). Commands declared with <calypsoCommands> get their group from their declaration."

	^ ClyRootMenuGroup
]

{ #category : 'menu' }
ClyCommand >> menuSubmenuFor: aCommandActivator [
	"The sub-menu this command opens in a context menu, or nil when it is a plain item. An empty menu means that the item is not shown."
	^ nil
]

{ #category : 'accessing' }
ClyCommand >> morphicUIManager [

	^ MorphicUIManager new
]

{ #category : 'accessing' }
ClyCommand >> name [
	^ self class name
]

{ #category : 'accessing' }
ClyCommand >> order [
	"The order of the command in a menu when the entry that declares it gives none, see ClyCommandEntry>>#menuItemOrderFor:. It is the default order of menu activations."

	^ 1000
]

{ #category : 'accessing' }
ClyCommand >> orderInContext: aToolContext [

	^ self order
]

{ #category : 'executing' }
ClyCommand >> prepareFullExecution [
	^ self
]

{ #category : 'accessing' }
ClyCommand >> prepared [
	^ prepared
]

{ #category : 'accessing' }
ClyCommand >> prepared: aBoolean [
	prepared := aBoolean
]

{ #category : 'execution' }
ClyCommand >> processCommandFailure: anException [

	context processFailure: anException of: self
]

{ #category : 'registering' }
ClyCommand >> registerActivationItem: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName [
	"doRegisterContextMenuItemsFor:withBuilder: of an activation item."
	| activator itemOrder item |
	activator := aPresentedCommand activator.
	itemOrder := aPresentedCommand order.
	(aGroup descriptor isRoot not and: [ aGroup descriptor isInlined ]) ifTrue: [ itemOrder := aGroup order + (itemOrder / 10000000.0) ].
	item := (aBuilder item: aPresentedCommand dynamicName)
		parent: actualName;
		order: itemOrder;
		help: aPresentedCommand description;
		action: [ activator executeCommand ].
	activator setUpShortcutTipForMenuItem: item.
	self setUpIconForMenuItem: item
]

{ #category : 'registering' }
ClyCommand >> registerPresented: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName [
	"The registration of an activation item in a pragma menu builder."
	self registerActivationItem: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName
]

{ #category : 'accessing' }
ClyCommand >> secureMenuIcon [
	"In case when icon should be mandatory property of command
	users should call this method. It ensure Form instance
	which will indicate missing icon as a problem"

	^self defaultMenuIcon ifNil: [ Smalltalk ui iconNamed: #error ]
]

{ #category : 'context menu' }
ClyCommand >> setUpIconForMenuItem: menuItem [

	self defaultMenuIconFormSet ifNotNil: [:iconFormSet |
		menuItem iconFormSet: iconFormSet ]
]

{ #category : 'context menu' }
ClyCommand >> setUpIconNamed: aString forMenuItem: aMenuItemMorph [

	aMenuItemMorph icon: (aMenuItemMorph iconNamed: aString)
]

{ #category : 'accessing' }
ClyCommand >> specUIManager [

	^ SpAlertDialog new
]

{ #category : 'toolbar' }
ClyCommand >> toolbarItemFor: aCommandActivator [
	"The widget this command puts in a toolbar for its activator, or nil for the default toolbar button."
	^ nil
]
