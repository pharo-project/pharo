"
I am the root of the Calypso commands, Commander2 commands run in a tool context (ClyToolContext, see #context).

Subclasses implement #execute, and usually #name, #iconName, and #canBeExecuted, calling super, to check the selection of their context. The first paragraph of the class comment of a command is its tooltip (#description), so start it with a short sentence for users and put the developer details in the following paragraphs.

A command does not say where it is shown: anchors declare it in their menus, toolbars, shortcuts... with class-side methods marked with the <calypsoCommands> pragma (see ClyCommandMap). The guide doc/Calypso/Commands.md of the Pharo repository explains how to write and declare commands, and doc/Calypso/CommandAnnotationsMigration.md how to migrate commands placed by class annotations.

I also keep the helper methods that the Calypso morphs (toolbar, tabs, table cells, context menus) call on a command.

Moving Calypso to Spec

The command declarations do not depend on Morphic. They are anchored on tool context classes and item types, not on morphs, so they stay valid when a pane becomes a Spec presenter. A Spec tool asks the same API as the Morphic one: ClyCommandRegistry default activationsOf: #contextMenu inContext: aToolContext, then ClyCommandGroupBuilder new buildMenuOf: activations context: aToolContext pureRoots: #(). The tree it answers is made of ClyPresentedGroup (a CmCommandGroup, its #descriptor is the ClyMenuGroup) and ClyPresentedCommand (a CmUICommand, with its #activator). ClyCommandGroupMenuRenderer is the Morphic renderer of that tree. A Spec tool needs a Spec renderer instead.

A spike (November 2026) gave a Spec list of methods the context menu and the shortcuts of Calypso through #actions:, with one SpActionGroup, and its menu was identical to the Morphic menu in the method contexts compared. How it was built:
- The context menu is built each time it opens, because the commands depend on the selection. A Spec menu is a list of groups separated by lines, and a group nested in another group is merged into it (SpDynamicActionGroup too), so the separators are lost. The spike used a subclass of SpActionGroup whose #acceptVisitor: fills a new SpActionGroup new beRoot from the registry and lets the visitor visit it: the groups of a root become the groups of the menu.
- The tree is translated with one Spec group (beDisplayedAsGroup) for each run of commands and sub-menus, and one for each inlined ClyMenuGroup (#isInlined). Other groups become sub-menus (beDisplayedAsSubMenu) with the #name, #description and #iconName of the ClyMenuGroup. Inactive groups and commands (#isActive) are skipped, as ClyCommandGroupMenuRenderer does.
- A command becomes an SpAction with the #menuItemName of its activator, the #description and #iconName of the command, the action [ activator executeCommand ], and the shortcutKey of the first shortcut of the command in the context (ClyCommandRegistry>>#shortcutsOf:inContext:). A command with #menuAlternatives (the scope switcher) gives one action per alternative (activator newForCommand: each).
- Spec installs the key bindings once, when the presenter is built (SpAbstractMorphicAdapter>>#addKeyBindingsTo:), while the commands of a key depend on the selection. The spike bound one shortcut-only action per key (SpActionGroup>>#addShortcutWith:) which runs the first active activation of #shortcuts with that key in the current context, as ClyKMDispatcher does.

What the spike found to solve first:
- Contexts need a browser morph. A ClyBrowserContext holds its browser as its tool, and commands ask it for the system, the navigation scopes (#refactoringScopes) and use it to show their results. The spike kept a ClyQueryBrowserMorph which is never opened. Contexts also hold data source items, made without data source with ClyDataSourceItem of: nil value: anObject asCalypsoBrowserItem. A context which does not need a morph is the first step of the migration.
- Commands declared on context classes (ClyBrowserContext and its subclasses) belong to the browser: through such a context, a Spec tool also gets Meta+W, which closes the browser, and Alt+Left/Right, which navigate its history. Commands declared on item types (ClyMethod, ClyClass, Package...) carry over as they are.
- Keys are dispatched differently. ClyKMDispatcher tries the active shortcuts of the context in order for each typed key, and the first one which matches the keys typed so far, wholly or partly, wins. Spec (KeyMapping) waits as soon as the typed keys begin a longer bound sequence. Binding every declared key broke Meta+M (Meta+M Meta+C is declared for classes) and Meta+T (Meta+T Meta+O is declared for methods). The spike bound only the keys declared for an example context of the tool, without those shadowed by an earlier key, which is right for one tool but not in general.
- Spec lists have their own Meta+T, which shows the context menu (SpAbstractListPresenter>>#registerActions), and it wins over the Meta+T of Calypso, which opens a sub-menu.
- ClyOpenMenuGroupCommand, which the shortcuts and tab actions declared with #addMenuOf: run, opens a modal Morphic menu on the tool of its context. It needs a Spec version.
- Spec sub-menus do not show the key which opens them (ClyCommandGroupMenuRenderer>>#keyTextOfGroup:). Commands which draw their own icon by overriding #defaultMenuIconFormSet, such as SycGenerateEqualAndHashCommand, need an icon provider since Spec actions take an icon name.

Not tried by the spike, still Morphic: the toolbar (ClyCommandGroupMenuRenderer>>#renderToolbarOf:on:, and the widgets of #toolbarItemFor:, such as in ClySwitchBrowserModeCommand and ClySwitchQueryScopeCommand), the tab actions (ClyCommandGroupMenuRenderer>>#notebookActionsOf:), the table icons (ClyCommandBasedTableDecorator, #decorateTableCell:using:), the clicks and double clicks (ClyQueryViewMorph), drag and drop (ClyDragAndDropCommandActivator, #dropContext), the left bar of text editors (ClyMouseInteractionHandler) and the Morphic sub-menus of #menuSubmenuFor:. The browser tabs and table decorations are still placed by class annotations (ClyTabActivationStrategyAnnotation, ClyTableDecorationStrategyAnnotation).
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
	"Deprecated: commands check their context in the instance-side #canBeExecuted. I am kept for the commands of other projects which still check it here, see doc/Calypso/CommandAnnotationsMigration.md of the Pharo repository."

	^ self isAbstract not
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
	"Whether I can run in my context. Commands override me, calling super, to check their context. The class-side #canBeExecutedInContext: is still asked for the commands of other projects that check their context there."

	^ self class canBeExecutedInContext: self context
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
	"Answer the first paragraph of my class comment. Tools show it as the tooltip of my menu items, so it should be a short sentence for users. Developer details go in the following paragraphs."

	| lines separatorIndex |
	lines := self class comment ifEmpty: [ ^ nil ] ifNotEmpty: [ :comment | comment trimBoth lines ].
	separatorIndex := lines findFirst: [ :line | line isAllSeparators ].
	separatorIndex > 0 ifTrue: [ lines := lines first: separatorIndex - 1 ].
	^ String cr join: lines
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

	self defaultMenuIconFormSet ifNotNil: [ :iconFormSet | menuItem iconFormSet: iconFormSet ]
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
