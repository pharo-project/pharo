*GT-SpotterExtensions-Core
gtExampleMenuItemMorphWithoutSubitems
	<gtExample>
	<label: 'Menu item without subitems'>
	
	^ World worldMenu items 
		detect: [ :each | each contents = 'System Browser' ]