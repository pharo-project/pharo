"
Provide help topic for world menu
"
Class {
	#name : #WorldMenuHelp,
	#superclass : #CustomHelp,
	#category : #'Pharo-Help'
}

{ #category : #accessing }
WorldMenuHelp class >> bookName [ 
	^'World Menu Items'
]

{ #category : #pages }
WorldMenuHelp class >> overview [
	^ HelpTopic 
		title: 'Overview'
		contents: 
'The World Menu is acceesible by clicking on the background of Pharo. The menu provides shortcuts to launch tools. New menu items can be easily added. This overview gives the basic instruction how.

Consider you have a class GAPlugin. You can add a world menu item with the following _class-size_ method:
-=-=-=-=-=-=-=-=-=-=-=-=
GAPlugin class>>menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #GeneticAlgo) action: [ 42 inspect ]
-=-=-=-=-=-=-=-=-=-=-=-=

The code given above creates a menu entry called "GeneticAlgo". Selecting this item executes the code provided to #action:. 
Submenus can also be created:

-=-=-=-=-=-=-=-=-=-=-=-=
GAPlugin class>>menuCommandOn: aBuilder
	<worldMenu>
	aBuilder item: #GeneticAlgo.
	
	((aBuilder
		item: ''Loading Graphical Examples'';
		parent: #GeneticAlgo))
	action: [ 42 inspect ]
-=-=-=-=-=-=-=-=-=-=-=-=

The code above creates a menu in the world menu and add an entry into it.

'
]

{ #category : #accessing }
WorldMenuHelp class >> pages [
	^#(overview)
]
