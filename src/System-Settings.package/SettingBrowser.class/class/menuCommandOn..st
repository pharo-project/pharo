world menu
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #Settings)
		parent: #System;
		order: 1;
		iconName: #smallConfigurationIcon;
		action: [ SettingBrowser open ];
		keyText: 'o, s';
		help:
			'Opens a SystemSettingBrowser which allows you to alter many system settings'