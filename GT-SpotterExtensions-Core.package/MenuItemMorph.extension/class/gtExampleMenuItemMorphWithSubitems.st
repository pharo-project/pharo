*GT-SpotterExtensions-Core
gtExampleMenuItemMorphWithSubitems
	<gtExample>
	<label: 'Menu item with subitems'>
	
	^ World worldMenu items 
		detect: [ :each | each contents = 'System' ]