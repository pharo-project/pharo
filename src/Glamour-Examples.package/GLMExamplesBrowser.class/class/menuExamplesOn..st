user interface
menuExamplesOn: aBuilder
	"Add a custom menu item to the world menu"

	<worldMenu>
	(aBuilder item: #'Glamour Example Browser')
		iconName: #glamorousHelp;
		parent: #Help;
		help: 'Open Glamour Example Browser';
		action: [ self open ]