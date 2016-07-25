system menu
systemReporterMenuOn: aBuilder 
	"I build a menu"
	<worldMenu>
	(aBuilder item: #'System Reporter')
		parent: #System;
		action: [ self open ];
		help: 'If you have a bug, use this tool to provide information about your system'.