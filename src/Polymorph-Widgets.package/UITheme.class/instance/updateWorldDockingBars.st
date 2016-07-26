initialize-release
updateWorldDockingBars
	"Update the world docking bar gradients."
	
	World mainDockingBars do: [:d |
		d
			color: (self worldMainDockingBarColorFor: d);
			gradientRamp: (self class current
				worldMainDockingBarNormalFillStyleFor: d) colorRamp;
			borderStyle: (self worldMainDockingBarBorderStyleFor: d)]