# Calypso commands

This guide explains how commands are added to Calypso, the system browser: its context menus, toolbars, tab actions, table icons, shortcuts, clicks, drag and drop and the left bar of its code editors.

Everything goes through one mechanism: class-side methods marked with the `<calypsoCommands>` pragma, which declare the commands of an anchor. If you used class annotations (`ClyContextMenuActivation`, `ClyShortcutActivation`...) before, read [Migrating Calypso commands from class annotations](CommandAnnotationsMigration.md) too.

- [A first example](#a-first-example)
- [Writing a command](#writing-a-command)
- [Anchors: where commands apply](#anchors-where-commands-apply)
- [Declaration methods](#declaration-methods)
- [Interactions](#interactions)
- [Menu groups and sub-menus](#menu-groups-and-sub-menus)
- [Toolbar groups](#toolbar-groups)
- [When a command is shown and when it runs](#when-a-command-is-shown-and-when-it-runs)
- [Finding where a command is declared](#finding-where-a-command-is-declared)
- [Using the commands in a tool](#using-the-commands-in-a-tool)
- [Testing](#testing)
- [Classes involved](#classes-involved)

## A first example

A plugin adds a "Show coverage" item to the context menu of methods and binds it to ⇧⌘K. First the command:

```smalltalk
ClyCommand << #MyShowCoverageCommand
	package: 'MyPlugin-Calypso'
```

```smalltalk
MyShowCoverageCommand >> name

	^ 'Show coverage'
```

```smalltalk
MyShowCoverageCommand >> iconName

	^ #smallInspectIt
```

```smalltalk
MyShowCoverageCommand >> execute

	MyCoverageBrowser openOn: self context selectedMethods
```

Its class comment starts with the sentence users see as the tooltip:

```
Show which lines of the selected methods are covered by tests.

I open MyCoverageBrowser on the selected methods.
```

Then the declaration, an extension method of the plugin on the class side of `ClyMethod`:

```smalltalk
ClyMethod class >> myPluginCommandsIn: aCommandMap

	<calypsoCommands>
	aCommandMap contextMenu add: MyShowCoverageCommand order: 1000.
	aCommandMap shortcuts add: MyShowCoverageCommand key: $k meta shift
```

As soon as the method is compiled, the item is in the context menu of every list showing methods, with ⇧⌘K displayed next to it, and ⇧⌘K runs it.

## Writing a command

Calypso commands subclass `ClyCommand`, a Commander2 command (`CmCommand`). The command is created for each context it may be shown in, so it can ask its context (`self context`, a `ClyToolContext`) about the selection.

| Method | Purpose | Default |
|---|---|---|
| `#execute` | Does the work. | |
| `#name` | The label of the menu item. | The class name |
| `#defaultToolbarItemName` | The label of the toolbar button. | `#name` |
| `#iconName` | The name of the icon of the menu item, toolbar button, tab action or table icon. Override `#defaultMenuIconFormSet` for an icon which is not a named one. | None |
| `#description` | The tooltip. It answers the first paragraph of the class comment, so write a short sentence for users first, and the developer details in the following paragraphs. | First paragraph of the class comment |
| `#canBeExecuted` | Whether the command can run in its context. Override it and call `super`. | Not abstract |
| `#isAvailableInMenuOf:` | Whether the command is shown in the menus, toolbars, tab actions and table icons of a context, even if it can run there, for example from a shortcut. | `true` |
| `#order` / `#orderInContext:` | The order of the item when its declaration gives none. | `1000` |
| `#prepareFullExecution` | Asks the user what the command needs (a new name, a target class...) before `#execute`. | Nothing |

For example, a command which only applies to one selected method:

```smalltalk
MyShowCoverageCommand >> canBeExecuted

	^ super canBeExecuted and: [ self context selectedMethods size = 1 ]
```

The commands of the system (refactorings, removals, renames...) live in the `SystemCommands-*` packages and subclass families such as `SycMethodCommand`, `SycSingleClassCommand` or `SycSourceCodeCommand`. Their refactoring subclasses (`SycMethodRefactoringCommand`, `SycClassRefactoringCommand`...) build `RBRefactoring` instances and show the changes before applying them.

## Anchors: where commands apply

The class whose class side holds the declaration is its anchor. It says in which contexts the commands apply. An anchor is one of:

- **a tool context class**, such as `ClyFullBrowserClassContext` or `ClyMethodSourceCodeContext`: the commands apply to the contexts which are instances of it or of its subclasses.
- **an item type**, such as `ClyMethod`, `ClyClass`, `Package`, `ClyInstanceVariable` or `ClyTaggedClassGroup`: the commands apply to every browser context whose selected items are of this type, whatever the browser or the pane.

Prefer an item type when the command is about the selected items wherever they are shown, for example "Remove method" is declared on `ClyMethod`, so it is in the method pane of the full browser and in every query browser. Use a context class when the command only makes sense in one tool, for example "Rename tag" is declared on `ClyFullBrowserClassGroupContext`.

`ClyToolContext>>#isDescribedByCommandAnchor:` decides whether an anchor describes a context.

## Declaration methods

A declaration method:

- is on the class side of the anchor;
- is marked with the `<calypsoCommands>` pragma (see `ClyCommandMap class>>#pragmaName`);
- takes one argument, the `ClyCommandMap` of the anchor, and adds commands to its interactions;
- is often an extension method, so the package of the commands declares them on an anchor of another package. Name it after the package, such as `systemCommandsMethodCommandsIn:`, so two packages never override each other.

```smalltalk
ClyFullBrowserPackageContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap

	<calypsoCommands>
	(aCommandMap toolbar group: ClyPackageToolbarGroup)
		add: ClySwitchToPackagesCommand order: 1;
		add: ClySwitchToProjectsCommand order: 10.
	aCommandMap shortcuts
		add: SycRemovePackageCommand key: $x meta;
		add: SycRenamePackageCommand key: $r meta.
	aCommandMap contextMenu
		add: SycAddNewPackageCommand;
		add: SycRemovePackageCommand;
		add: SycRenamePackageCommand
```

The declarations are plain data: they are run once without any tool context, in the order of their selectors, and kept in a cache by `ClyCommandRegistry`. The cache is reset when a declaration method or a command class changes, so a new declaration shows at once.

Each `add:` answers the new entry (`ClyCommandEntry`), which accepts:

| Message | Effect |
|---|---|
| `order: aNumber` | The place of the item in its group, smaller first. Without it, the command's `#orderInContext:` is used. Items of the same order are sorted by name. |
| `label: aString` | The label of the item, instead of the command's `#name` (or `#defaultToolbarItemName` in toolbars). |
| `priority: aNumber` | When several commands of a context share a key, a click or a drop, the active one of the highest priority runs. In table icons, the command of the highest priority decorates the cell last, so its icon is the one shown. Default `0`. |

Declare each command explicitly. Only use `addAllConcreteSubclassesOf:` for a closed family whose hierarchy is the menu, such as the view modes of the class pane:

```smalltalk
(aCommandMap toolbar group: ClyClassHierarchyToolbarGroup) addAllConcreteSubclassesOf: ClySwitchClassViewModeCommand
```

## Interactions

A command map has one interaction per way of reaching a command:

| Interaction | Shown or run by | Declaring |
|---|---|---|
| `contextMenu` | The context menu of lists and code editors | `add:`, `add:order:` |
| `toolbar` | The toolbar of the browsers, in a `ClyToolbarGroup` | `(toolbar group: AToolbarGroup) add:order:` |
| `tabActions` | The action buttons of the browser tabs | `add:`, `addMenuOf:` |
| `tableIcons` | The icons in the cells of the lists | `(add:) priority:` |
| `shortcuts` | A key combination | `add:key:`, `addMenuOf:key:` |
| `click` / `doubleClick` | A click or double click on an item of a list | `add:` |
| `dragAndDrop` | Dropping the selected items on another item | `add:dropIn:` |
| `textLeftBarMenu` | The menu of the left bar of code editors | `add:` |
| `textLeftBarClick` / `textLeftBarDoubleClick` | A click or double click in the left bar of code editors | `add:` |

Shortcuts take a `KMKeyCombination`, such as `$x meta` or `$t meta shift`. The key of a shortcut is shown next to the menu item of the same command.

Mouse entries use the left (red) button by default. Send `beYellowButton` or `beBlueButton` for the other buttons, and `keyboardModifier:` (a `KMModifier`) when a key must be held:

```smalltalk
(aCommandMap textLeftBarClick add: ClyOpenTextLeftBarMenuCommand) beYellowButton
```

For drag and drop, the anchor of the declaration describes the dragged items and the drop anchor the item they are dropped on:

```smalltalk
aCommandMap dragAndDrop add: SycMoveMethodsToClassCommand dropIn: ClyClass
```

The dragged command gets its drop context with `#dropContext`.

## Menu groups and sub-menus

Commands declared without a group are at the root of the menu (`ClyRootMenuGroup`). Every other group is a subclass of `ClyMenuGroup`:

| Method | Purpose | Default |
|---|---|---|
| `#name` | The label of the sub-menu | The class name |
| `#order` | The place of the group among the items of its parent | `500` |
| `#isInlined` | `true` to show the commands in the parent menu, between lines, instead of a sub-menu | `false` |
| `#hasLineBefore` / `#hasLineAfter` | Lines around the sub-menu | `false` |
| `#iconName`, `#description` | The icon and tooltip of the sub-menu | None |
| `class >> #parentGroup` | The group it is nested in | `ClyRootMenuGroup` |
| `#prepareToOpenIn:` | Run before a shortcut or a tab action opens the group as a pop-up menu | Nothing |

`(aCommandMap contextMenu group: AMenuGroup)` answers a view of the interaction which adds its commands to the group:

```smalltalk
(aCommandMap contextMenu group: SycMethodRefactoringMenuGroup)
	add: SycPullUpMethodCommand order: 10;
	add: SycPushDownMethodCommand order: 20
```

Any package can add commands to a group declared by another one: the group is a class, not a place in one declaration.

A shortcut or a tab action can open a sub-menu of the context menu on its own as a pop-up menu, with `addMenuOf:`. It adds a `ClyOpenMenuGroupCommand`, whose name, icon and tooltip are the ones of the group, and the key is shown next to the sub-menu in the context menu:

```smalltalk
aCommandMap shortcuts addMenuOf: SycMethodRefactoringMenuGroup key: $t meta.
aCommandMap tabActions addMenuOf: SycClassRefactoringMenuGroup
```

## Toolbar groups

Toolbar commands are placed in subclasses of `ClyToolbarGroup`, ordered by their `#order` and name, such as `ClyClassHierarchyToolbarGroup` or `ClyPackageToolbarGroup`. A command can put its own widget in the toolbar instead of a button by overriding `#toolbarItemFor:`, as `ClySwitchBrowserModeCommand` and `ClySwitchQueryScopeCommand` do.

## When a command is shown and when it runs

For an interaction and a context, `ClyCommandRegistry` takes the entries declared on every anchor describing the context, sorted by priority. Then each entry creates its command in the context:

- an entry is **active** when its command answers `true` to `#canBeExecuted`;
- in the menus, toolbars, tab actions, table icons and the left bar menu, the command must also answer `true` to `#isAvailableInMenuOf:`. Inactive commands are not shown;
- for a shortcut, a click or a drop, the first active entry runs.

So `#canBeExecuted` says whether the command makes sense for the selection, and `#isAvailableInMenuOf:` hides a command from the menus of a context where it is reached another way, such as a shortcut.

## Finding where a command is declared

Every entry remembers its declaration method:

```smalltalk
"Every declared command"
ClyCommandRegistry default allEntries.

"Where a command is declared"
(ClyCommandRegistry default allEntries select: [ :entry | entry commandClass = SycRemoveMethodCommand ]) collect: [ :entry | entry declaringMethod ].

"The context menu entries of a context"
ClyCommandRegistry default activationsOf: #contextMenu inContext: aToolContext.
```

The shortcuts browser of NewTools lists the declared shortcuts and browses their declaration method.

Overriding or removing a command declared by another package is not supported yet.

## Using the commands in a tool

A tool showing Calypso commands asks the registry for the interaction, builds the tree of groups and renders it:

```smalltalk
| activations tree |
activations := ClyCommandRegistry default activationsOf: #contextMenu inContext: aToolContext.
tree := ClyCommandGroupBuilder new buildMenuOf: activations context: aToolContext pureRoots: #().
^ ClyCommandGroupMenuRenderer new renderTree: tree on: aToolContext tool
```

Other entry points of `ClyCommandRegistry`: `#activatorsOf:inContext:` for the commands that can run, `#executeFirstCommandOf:inContext:` for a click, `#shortcutsOf:inContext:` for the keys of a command. `ClyKMDispatcher` runs the shortcuts of a morph and `ClyMouseInteractionHandler` its clicks.

## Testing

A test can build a registry limited to some anchors, so it does not depend on the commands of the image:

```smalltalk
registry := ClyCommandRegistry forAnchors: { MyMockToolContext }.
registry activationsOf: #contextMenu inContext: MyMockToolContext new
```

See `ClyCommandRegistryTest` and `ClyCommandMapTest` in `Calypso-Commands-Tests`.

## Classes involved

| Class | Role |
|---|---|
| `ClyCommand` | Root of the Calypso commands |
| `ClyToolContext` | The context a command runs in |
| `ClyCommandMap` | The commands of one anchor, by interaction |
| `ClyCommandInteraction` | The commands of one interaction of a map, the receiver of `add:` |
| `ClyCommandEntry` and its subclasses `ClyShortcutCommandEntry`, `ClyMouseCommandEntry`, `ClyDropCommandEntry` | One declared command and its placement |
| `ClyCommandRegistry` | Finds the declarations, caches the maps, answers the commands of a context |
| `ClyMenuGroup`, `ClyRootMenuGroup`, `ClyToolbarGroup` | Groups of menus and toolbars |
| `ClyOpenMenuGroupCommand` | Opens a menu group as a pop-up menu |
| `ClyCommandGroupBuilder`, `ClyCommandGroupMenuRenderer` | Build and render the menu trees |
