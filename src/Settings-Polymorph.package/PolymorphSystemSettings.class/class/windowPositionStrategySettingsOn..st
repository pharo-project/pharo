morphic
windowPositionStrategySettingsOn: aBuilder
	<systemsettings>
	(aBuilder pickOne: #usedStrategy) 
		label: 'Window position strategy';
		parent: #windows;
		target: RealEstateAgent;
		domainValues: {'Reverse Stagger' -> #staggerFor:initialExtent:world:. 'Cascade' -> #cascadeFor:initialExtent:world:. 'Standard' -> #standardFor:initialExtent:world:};
		description: 'If Reversed Stagger, windows are placed in free space,
if Cascade, windows are placed in cascade based on current active window or based on cursor position.' .