world menu
menuCommandOn: aBuilder
	"Add a custom menu item to the world menu"

	<worldMenu>
	(aBuilder item: #'Catalog Browser')
		order: 0.19;
		iconName: #catalogIcon;
		parent: #Tools;
		action: [ self open ]