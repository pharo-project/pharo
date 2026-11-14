# Migrating Calypso commands from class annotations

Until Pharo 15, Calypso commands were placed in menus, toolbars and shortcuts by class annotations: class-side methods marked with `<classAnnotation>` answering an activation strategy such as `ClyContextMenuActivation` or `ClyShortcutActivation`. Some menus were also built from root command classes (`ClyToolContext>>#commanderCommandClasses`) and opener commands.

Pharo 15 replaces all of them by declarations with the `<calypsoCommands>` pragma, described in [Calypso commands](Commands.md).

The activation strategies still work: they moved to the deprecated `Commander-Calypso-Compatibility` package of the Commander baseline, and Calypso shows their commands next to the declared ones. **That package will be removed in Pharo 16**, so projects adding commands to Calypso should migrate before.

- [What changes](#what-changes)
- [Annotations to declarations](#annotations-to-declarations)
- [Other changes to commands](#other-changes-to-commands)
- [Converting a package automatically](#converting-a-package-automatically)
- [Checking the result](#checking-the-result)

## What changes

Before, the command said where it was shown:

```smalltalk
SycRemoveMethodCommand class >> methodContextMenuActivation

	<classAnnotation>
	^ ClyContextMenuActivation byRootGroupItemOrder: 10000 for: ClyMethod asCalypsoItemContext
```

```smalltalk
SycRemoveMethodCommand class >> methodShortcutActivation

	<classAnnotation>
	^ ClyShortcutActivation by: $x meta for: ClyMethod asCalypsoItemContext
```

Now the anchor lists its commands, and a package can declare commands on an anchor of another package with an extension method:

```smalltalk
ClyMethod class >> systemCommandsMethodCommandsIn: aCommandMap

	<calypsoCommands>
	aCommandMap contextMenu add: SycRemoveMethodCommand order: 10000.
	aCommandMap shortcuts add: SycRemoveMethodCommand key: $x meta
```

The context of an annotation becomes the anchor of the declaration:

| Annotation context | Anchor |
|---|---|
| A tool context class, such as `ClyFullBrowserClassContext` | The same class |
| `ClyMethod asCalypsoItemContext`, `Package asCalypsoItemContext`... | The item type: `ClyMethod`, `Package`... |

Put all the commands of a package for one anchor in one declaration method, named after the package.

## Annotations to declarations

| Annotation | Declaration |
|---|---|
| `ClyContextMenuActivation byRootGroupItemFor: ctx` | `aCommandMap contextMenu add: Cmd` |
| `ClyContextMenuActivation byRootGroupItemOrder: 10 for: ctx` | `aCommandMap contextMenu add: Cmd order: 10` |
| `ClyContextMenuActivation byItemOf: AGroup order: 10 for: ctx` | `(aCommandMap contextMenu group: AGroup) add: Cmd order: 10` |
| `ClyContextMenuActivation byItemOf: AGroup for: ctx` | `(aCommandMap contextMenu group: AGroup) add: Cmd` |
| `byItem: 'Label' of: AGroup order: 10 for: ctx` | `((aCommandMap contextMenu group: AGroup) add: Cmd order: 10) label: 'Label'` |
| `ClyShortcutActivation by: $x meta for: ctx` | `aCommandMap shortcuts add: Cmd key: $x meta` |
| `CmdShortcutActivation removalFor: ctx` | `aCommandMap shortcuts add: Cmd key: $x meta` |
| `CmdShortcutActivation renamingFor: ctx` | `aCommandMap shortcuts add: Cmd key: $r meta` |
| `ClyToolbarActivation byItemOf: AToolbarGroup order: 10 for: ctx` | `(aCommandMap toolbar group: AToolbarGroup) add: Cmd order: 10` |
| `ClyBrowserTabCommandActivation for: ctx` | `aCommandMap tabActions add: Cmd` |
| `ClyTableIconCommandActivation priority: 100 for: ctx` | `(aCommandMap tableIcons add: Cmd) priority: 100` |
| `ClyClickActivation for: ctx` | `aCommandMap click add: Cmd` |
| `ClyDoubleClickActivation for: ctx` | `aCommandMap doubleClick add: Cmd` |
| `ClyDragAndDropActivation for: dragCtx toDropIn: dropCtx` | `aCommandMap dragAndDrop add: Cmd dropIn: DropAnchor` on the drag anchor |
| `ClyTextLeftBarMenuActivation for: ctx` | `aCommandMap textLeftBarMenu add: Cmd` |
| `ClyTextLeftBarClickActivation byYellowButtonFor: ctx` | `(aCommandMap textLeftBarClick add: Cmd) beYellowButton` |
| `ClyTextLeftBarDoubleClickActivation for: ctx` | `aCommandMap textLeftBarDoubleClick add: Cmd` |
| An annotation with a priority, such as `(... for: ctx) priority: 1000` | `(... add: Cmd) priority: 1000` |
| `CmdContextMenuActivation`, `CmdShortcutActivation` (Commander 1) | Same as their Calypso counterparts |
| `ClyTabActivationStrategyAnnotation for: ctx` on a tool | `aCommandMap tabs add: Tool` |
| `ClyTableDecorationStrategyAnnotation for: ctx` on a decorator | `aCommandMap tableDecorators add: Decorator` |

When an abstract command, tool or decorator declares an annotation inherited by all its concrete subclasses, declare each subclass. For a closed family whose hierarchy is the menu, such as view modes, use `addAllConcreteSubclassesOf:` instead.

### Sub-menus opened by a command

Sub-menus were opened by commands annotated for the menu (`SycOpenMethodMenuCommand`, `SycOpenClassMenuCommand`, `SycOpenSourceCodeMenuCommand`...), and their commands were placed with their own activation strategies (`SycMethodMenuActivation`, `SycClassMenuActivation`, `SycSourceCodeMenuActivation`...). They are now menu group classes, and a shortcut or tab action opening one is declared with `addMenuOf:`:

```smalltalk
(aCommandMap contextMenu group: SycMethodRefactoringMenuGroup) add: SycPullUpMethodCommand order: 10.
aCommandMap shortcuts addMenuOf: SycMethodRefactoringMenuGroup key: $t meta
```

The groups of the system are `SycMethodRefactoringMenuGroup`, `SycMethodExtraMenuGroup`, `SycClassRefactoringMenuGroup`, `SycClassExtraMenuGroup`, `SycClassScopesMenuGroup`, `SycPackageExtraMenuGroup`, `ClyMethodGroupExtraMenuGroup`, `SycSourceCodeMenuGroup`, `ClyMethodDebugPointsMenuGroup` and `ClySourceCodeDebugPointsMenuGroup`.

### Root command classes

`ClyToolContext>>#commanderCommandClasses` answered root classes whose concrete subclasses were all shown in the context menu. It is no longer asked by Calypso: declare each command in the context menu, in the menu group its class used to answer with `#menuGroup`.

### Contexts and providers

The other class annotations of Calypso become pragmas on the annotated class:

| Annotation | Declaration |
|---|---|
| `ClyContextSelectionStrategyAnnotation for: ABrowser selectionType: AnItemType` | `<calypsoSelectionContext>` method answering `ClySelectionContextDeclaration browser: ABrowser itemType: AnItemType` |
| `ClyContextSelectionStrategyAnnotation for: ABrowser` | `<calypsoSelectionContext>` method answering `ClySelectionContextDeclaration browser: ABrowser` |
| `(... ) priority: 1000` | `(ClySelectionContextDeclaration ...) priority: 1000` |
| `ClyInheritedScopeProviderAnnotation new priority: 100` | A method marked with `<calypsoInheritedScopeProvider: 100>` |
| `ClyMethodVisibilityProviderAnnotation new priority: 100` | A method marked with `<calypsoMethodVisibilityProvider: 100>` |

These annotations still work while `Commander-Calypso-Compatibility` is loaded. With them, `asCalypsoItemContext` and `ClyBrowserItemContext` moved to that package, and the `ClassAnnotation` packages are deprecated: they will be removed in Pharo 16 too.

## Other changes to commands

| Before | Now |
|---|---|
| `Cmd class >> canBeExecutedInContext: aToolContext` | `Cmd >> canBeExecuted`, which calls `super` and asks `self context`. The class-side method is still asked by `ClyCommand>>#canBeExecuted` for compatibility. |
| `Cmd >> menuGroup` overrides, used by root command classes | The group of the declaration |
| `Cmd >> icon` answering `self iconNamed: #x` | `Cmd >> iconName` answering `#x` |
| `Cmd >> description` answering a string | The first paragraph of the class comment |
| `Cmd >> isAvailableInMenuOf:` | Unchanged: it hides the command from menus, toolbars, tab actions and table icons |
| The `SycCmCommand` families (`SycMethodCmCommand`, `SycClassCmCommand`...) | Merged into the families of `SycMethodCommand`, `SycSingleClassCommand`, `SycSourceCodeCommand`... and renamed without `Cm`: `SycMethodRefactoringCommand`, `SycClassRefactoringCommand`, `SycGenerateCommand`, `SycMessageRefactoringCommand`, `SycVariableNavigationCommand`... |

## Converting a package automatically

`ClyCommandDeclarationGenerator`, in `Commander-Calypso-Compatibility`, converts the annotations of the commands of some packages into declaration methods, one per anchor and per package:

```smalltalk
generator := ClyCommandDeclarationGenerator forPackagesNamed: #( 'MyPlugin-Calypso' ).
generator commandPackagePrefixes: #( 'MyPlugin' ).
generator generateDeclarations.
generator mismatches. "Must be empty"
generator removeAnnotationMethods
```

`#commandPackagePrefixes:` names the projects of the commands to convert (Calypso and SystemCommands by default). An annotation method which also annotates commands of other projects, through inheritance, is not converted.

`#mismatches` compares each converted annotation with the entry declared for it, before the annotations are removed. `#undo` removes the generated declarations and compiles back the removed annotation methods. The annotations of opener commands are not converted: replace them by menu groups by hand.

Review the generated methods: they are formatted, but you may want to group or reorder the entries.

The generator converts the annotations of commands only. Convert the annotations of tabs, table decorators, contexts and providers by hand, following the tables above.

## Checking the result

To check that nothing moved, compare the menus before and after the conversion, for example by printing `ClyCommandRegistry default activationsOf: #contextMenu inContext: aContext` for the contexts of your plugin. The remaining annotations of the image are listed by:

```smalltalk
ClyCommandActivationStrategy allSubclasses flatCollect: [ :strategy | strategy registry allInstancesOf: strategy ]
```

and those of tabs and table decorators by `ClyCommandRegistry default legacyAnnotationsOf: #tabs` and `ClyCommandRegistry default legacyAnnotationsOf: #tableDecorators`.

Once a project has no annotation left, it no longer needs `Commander-Calypso-Compatibility`.
