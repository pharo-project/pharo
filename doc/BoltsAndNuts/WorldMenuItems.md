# World Menu Items

The World Menu is accessible by clicking on the background of Pharo. The menu provides shortcuts to launch tools. New menu items can be easily added. This overview gives the basic instruction how.

## Adding a menu item

Consider you have a class `GAPlugin`. You can add a world menu item with the following _class-side_ method:

```st
GAPlugin class >> menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #GeneticAlgo) action: [ 42 inspect ]
```

What is key there is that your class method is annotated with the `<worldMenu>` pragma.

The code given above creates a menu entry called `GeneticAlgo`.

Selecting this item executes the code provided to `action:` message.

## Adding submenus

Submenus can also be created:

```st
GAPlugin class>>menuCommandOn: aBuilder
	<worldMenu>
	aBuilder item: #GeneticAlgo.
	
	((aBuilder
		item: 'Loading Graphical Examples';
		parent: #GeneticAlgo))
	action: [ 42 inspect ]
```

The code above creates a menu in the world menu and add an entry into it.
