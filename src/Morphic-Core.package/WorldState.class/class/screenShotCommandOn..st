world menu items
screenShotCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #Screenshot)
		parent: #Tools;
		target: World;
		order: 0.59;
		selector: #makeAScreenshot;
		label: 'Screenshot';
		iconName: #smallScreenshotIcon