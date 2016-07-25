A MenuRegistration stores the declaration of a menu item. It is mainly an handler for a PluggableMenuItemSpec. 

A menu item is declared withing a particular method tagged with a pragma. This kind-of method takes a builder as argument and its evaluation results in the building of a MenuRegistration sub-tree which is stored in the builder.   As an example:
MenuRegistrationExample class>>myOwnCoolToolRegistrationOn: aBuilder 
	<worldMenuExample> 
	(aBuilder item: #MyOwnCoolTool)
		label: 'My own cool tool';
		target: Workspace; 
		selector: #openContents: ;
		arguments: #('yep, my own cool tool can be opened from the world menu ! :)')
	
Evaluating this method results in the creation of a MenuRegistration which name is #MyOwnCoolTool. Thus, the resulting sub-tree is only made of a root node. After it has be built, this root node is recorded in the builder. To experiment this, just evaluate the following code:
-------------
| builder |
builder := PragmaMenuBuilder new.
MenuRegistrationExample myOwnCoolToolRegistrationOn: builder.
builder explore
-------------

Using pragma allows the menu builder to dynamically discover which are the methods to evaluate in order to build a menu. Thus, a resulting menu is built by evaluating a set of methods which share the same pragma keyword.
In the following example, all method having <worldMenuExample> are evaluated for the building of the resulting menu:
---------------
(PragmaMenuBuilder pragmaKeyword: 'worldMenuExample' model: nil) menu popUpInWorld
---------------

Within a method, three kind of declarations can be used: (1) item by item (2) an item with a sub-menu and (3) a group.

1 -  item by item menu registration declaration:
This kind of declaring is for the setting of one menu item and only one within a method.

1.1 -  A simple menu item with an action
In the following example, a menu item with the name #'Browser' and a action which 
consists in sending #openClassBrowser to StandardToolSet is declared:

AClassSomewhere class>>openBrowserOn: aBuilder
	<myMenuTest>
	(aBuilder item: #'Browser') 
		target: StandardToolSet; 
		selector: #openClassBrowser.

A simple action without any argument can also be set with a block:
	(aBuilder item: #'Browser') action: [StandardToolSet openClassBrowser]

You can also indicate a balloon help string and a particular icon:
	(aBuilder item: #'Browser') 
		action: [StandardToolSet openClassBrowser];
		help: 'Open a system browser';
		icon: MenuIcons smallSystemBrowserIcon

If the action needs one or several arguments, you can also give it/them as follow:
	(aBuilder item: #'Save and quit') 
		target: SmalltalkImage current; 
		selector: #snapshot:andQuit:.
		arguments: #(true true)
		
By default, the item label is set with the item name but it can be explicitly given as follow:
	(aBuilder item: #'Browser')
		label: 'System browser'; 
		target: StandardToolSet; 
		selector: #openClassBrowser.

1.2  -  Placing the menu item in a  menu
The resulting menu item of previous example will be placed at the root of the menu.
In order to declare another place for it, you have to explicitly set its parent name.
As an example, consider the following item which declares a simple entry with no action.
Such item is typically used as a root for a sub-menu tree:

AClassSomewhere class>>openToolsOn: aBuilder
	<myMenuTest>
	(aBuilder item: #'Tools')

Now, a sub-menu item for #Tools can be declared separately, within another method
by using the #parent: message:

AnotherClassSomewhere class>>myToolsOn: aBuilder
	<myMenuTest>
	(aBuilder item: #'CoolTool')
		label: 'Cool tool';
		parent: #Tools;

Note that the argument of #parent: must be the name of another item. 
If it is not the case, then the parent name indication is simply ignored.

1.3  -  Item ordering

If no ordering setting is indicated, items ordering is unpredicable (it depends on method retrieving order). If one want an item to appear at a certain position, it is possible to set it by sending #order: to a MenuRegistration. The #order: message takes a float as argument. 
As an example, see  the two following declarations, in the resulting menu, Wozy is placed before 'Wozy configuration'

AnotherClassSomewhere class>>myWozySystemOpenOn: aBuilder
	<myMenuTest>
	(aBuilder item: #'Wozy')
		parent: #CoolTool;
		order: 1.0
		
AnotherClassSomewhere class>>myWozySystemConfigOn: aBuilder
	<myMenuTest>
	(aBuilder item: #'Wozy configuration')
		parent: #CoolTool;
		order: 2.0

2) Item with a submenu
The one menu item - one declaring method way can be ugly. When a set a menu items are known to be put all-together, it is possible to declare the sub-tree in one method. The following example show such a sub-tree with the #Tools item at root and four sub-items declared in a single method. Note a menu target declared for the root is shared by all sub-items. In that case, it also remains possible for a sub-item to declare its own target.

AClassSomewhere class>>openToolsOn: aBuilder
	<myMenuTest>
	(aBuilder item: #'Tools')
		target:  StandardToolSet; "The target is shared by all children"
		with: [ "My sub-menu are given here"
			(aBuilder item: #'System browser') selector: #openClassBrowser.
			(aBuilder item: #Workspace) selector: #openWorkspace.
			(aBuilder item: #'Test Runner') selector: #openTestRunner.
			(aBuilder item: #'Monticello Browser') selector: #openMonticelloBrowser]
		
3) group of menu items
When you want some items to be shown always grouped together, you can use a group. Its declaring is like an item with a submenu except that you are using the message #group: instead of #item:. The consequence is that only the children are shown in the menu. Of course, #label and #icon: are ignored for a group. Here is an example:

AClassSomewhere class>>mostUsedToolsOn: aBuilder
	<myMenuTest>
	(aBuilder group: #MostUsedTools) "My name can be also used as parent name"
		withSeparatorAfter; "A separator will be added after my last child"
		order: 0; "the entire group will be placed at the top"
		target:  StandardToolSet; "The target is shared by all children"
		with: [
			(aBuilder item: #'System browser') selector: #openClassBrowser.
			(aBuilder item: #Workspace) selector: #openWorkspace.
			(aBuilder item: #'Test Runner') selector: #openTestRunner.
			(aBuilder item: #'Monticello Browser') selector: #openMonticelloBrowser]

---------------------------
		
Instance Variables
	isGroup:		<Boolean>
	itemList:		<SortedCollection>
	order:		<Number>
	owner:		<MenuRegistration>
	parentName:		<Symbol>
	spec:		<PluggableMenuItemSpec>

isGroup
	- if true, then this item is ignored and flatten

itemList
	- all my MenuRegistration (my sub-menus or my elements if i'm a group)

order
	- my order in the owner list

owner
	- my owner

parentName
	- the declared parent name which serve as basis for PragmaBenuBuilder>>#arrangeRegistrations

spec
	- my PluggableMenuItemSpec
