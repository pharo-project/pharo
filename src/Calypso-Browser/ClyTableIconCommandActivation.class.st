"
I describe how to access and execute commands using iconic button in table row.
I implement way how to decorate table view with icons like in Nautilus method pane.
These icons are buttons based on commands which are marked with my instances.

Actual table decoration is delegated to command itself:

		command decorateTableCell: anItemCellMorph using: self

By default command just builds single iconic button using default menu icon. 
So you can simply implement #defaultMenuIconName on class side of command and it will be shown in the table.
But if you want special icon (different from menu) then you can redefine instance side method #tableCellIcon. 

At the end iconic button is added into the cell morph: 

	command decorateTableCell: anItemCellMorph with: aMorph

By default icon is added as cell definition morph:

	anItemCellMorph definitionMorph: aMorph

But command subclasses can define it as extra tool:

	anItemCellMorph addExtraTool: aMorph

Command subclasses can redefine default decoration logic and build completally different UI items instead of iconic button. They should override method:

- createTableCellButtonWith: anIcon using: aCommandActivator

Multiple commands can override decorated elements of each others. To manage such overrides you can specify priority for decoration:

	^ClyTableIconCommandActivation priority: 100 for: ClyMethodScope.

The command with most priority will decorate table at last order which means that their UI elements will present in table.

To implement correct order of my instances in registry I provide special sorted container for this. It manages instances in opposite order then default one
"
Class {
	#name : #ClyTableIconCommandActivation,
	#superclass : #CmdCommandActivationStrategy,
	#category : #'Calypso-Browser-Table'
}

{ #category : #private }
ClyTableIconCommandActivation class >> createContainerForRegistry [
	"This strategy decorates table rows with UI element which execute the command.
	Decoration with more priority should override applied changes of table row 
	which was produced by less prioritized decoration.
	It can be done if most priority command will decorate the table at last order.
	Here we revert annotation enumeration logic by managing registry in opposite order
	according to described idea"
	
	^SortedCollection sortBlock: #priority ascending
]

{ #category : #'instance creation' }
ClyTableIconCommandActivation class >> priority: aNumber for: aBrowserContext [
	"More priority value will allow command to override decorations from less priority commands"
	
	^(self for: aBrowserContext)
		priority: aNumber
]
