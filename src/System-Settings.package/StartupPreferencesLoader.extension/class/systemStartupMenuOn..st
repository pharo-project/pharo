*System-Settings-Browser
systemStartupMenuOn: aBuilder
	<worldMenu>
	(aBuilder item: #SystemStartup)
		label: 'Startup';
		parent: #System;
		order: 2;
		help: 'System startup related';
		iconName: #scriptManagerIcon