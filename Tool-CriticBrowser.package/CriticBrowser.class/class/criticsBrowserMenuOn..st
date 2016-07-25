menu
criticsBrowserMenuOn: aBuilder 
	"I build a menu"
	<worldMenu>
	(aBuilder item: 'Critic Browser')
		action: [ self openOnCurrentWorkingConfiguration];
		order: 0.41;
		parent: #Tools;
		help: 'To manage rule checks';
		icon: self icon