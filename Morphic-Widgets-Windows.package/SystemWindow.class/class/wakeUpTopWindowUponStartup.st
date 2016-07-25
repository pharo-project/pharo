top window
wakeUpTopWindowUponStartup
	TopWindow ifNotNil:
		[TopWindow isCollapsed ifFalse:
			[TopWindow model ifNotNil:
				[TopWindow model modelWakeUpIn: TopWindow]]]