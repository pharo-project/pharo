"
I am a root of hierarchy of table decorators.
My subclasses are responsible to decorate table rows/cells.
Currently my subclasses are not supposed to be instantiated. All behaviour are on class side.

To activate decorator it should be declared in the table decorators of the context or item type of the items where table should be decorated, with a class-side method marked with the <calypsoCommands> pragma (see ClyCommandMap>>#tableDecorators and the guide doc/Calypso/Commands.md of the Pharo repository):

	ClyClass class >> calypsoSystemPluginsInheritanceAnalysisBrowserCommandsIn: aCommandMap
		<calypsoCommands>
		aCommandMap tableDecorators add: ClyAbstractClassTableDecorator

Also decorators can decide what exact items they want decorate. They should override the method:
	 wantsDecorateTableCellOf: aDataSourceItem
By default it returns true.

Actual decoration logic should be implemented in following two methods:

- decorateMainTableCell: anItemCellMorph of: aDataSourceItem
- decorateTableCell: anItemCellMorph of: aDataSourceItem

Idea is to decorate all columns in the table. But the main column is special. It can be decorated with extra tools like icons and buttons.
Other columns are simple label and they can be decorated only by special label style.

So if you only need special label for your items then implement only last method.
But for complex logic you should define first one. 
Notice that in multi column case last method will accept cell morphs which do not support any icons decoration.

These methods simplify decoration in simple cases which depends only on current item state.
But there are other methods which allow decoration which depends on complex context of the full browser.
For example style of method can be different depending on the method and the class selected in the browser.
Following methods should be overridden in such cases: 

- decorateMainTableCell: anItemCellMorph inContext: itemContext 
- decorateTableCell: anItemCellMorph inContext: itemContext

By default they just call first pair of methods with selected item in context.

When table is building multiple decorators can override same cell properties. To manage these overrides I introduce priority which specifies what decoration is more important. More important decoration is evaluated at last time which overrides equal properties from other decorations.
The priority is defined by my subclasses in the class-side method #priority, and ClyCommandRegistry applies the decorators by ascending priority (see ClyTableDecoratorEntry).

To be able control set of decorators per browser instance subclasses can be attached to concrete browser plugins.
Browser instance is created with specific set of plugins which restrict number of decorators which affect browser tables.
To specify plugin following class side method should be overridden:
- browserPluginClass
By default any decorators is bound to ClyStandardBrowserPlugin
"
Class {
	#name : 'ClyTableDecorator',
	#superclass : 'Object',
	#category : 'Calypso-Browser-Table',
	#package : 'Calypso-Browser',
	#tag : 'Table'
}

{ #category : 'decoration' }
ClyTableDecorator class >> browserPluginClass [
	^ClyStandardBrowserPlugin
]

{ #category : 'decoration' }
ClyTableDecorator class >> decorateMainTableCell: anItemCellMorph inContext: itemContext [

	self decorateMainTableCell: anItemCellMorph of: itemContext lastSelectedItem
]

{ #category : 'decoration' }
ClyTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [

	self decorateTableCell: anItemCellMorph of: aDataSourceItem
]

{ #category : 'decoration' }
ClyTableDecorator class >> decorateTableCell: anItemCellMorph inContext: itemContext [

	self decorateTableCell: anItemCellMorph of: itemContext lastSelectedItem
]

{ #category : 'decoration' }
ClyTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
	self subclassResponsibility
]

{ #category : 'testing' }
ClyTableDecorator class >> isAbstract [
	^self = ClyTableDecorator
]

{ #category : 'decoration' }
ClyTableDecorator class >> priority [
	"Decorators can override the properties of table cells set by others: the ones of larger priority are applied last, so their changes win. See ClyTableDecoratorEntry."

	^ 0
]

{ #category : 'testing' }
ClyTableDecorator class >> wantsDecorateTableCellInContext: singleItemContext [
	(singleItemContext browser hasPluginOf: self browserPluginClass)
		ifFalse: [ ^false ].

	^self wantsDecorateTableCellOf: singleItemContext lastSelectedItem
]

{ #category : 'testing' }
ClyTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^true
]
