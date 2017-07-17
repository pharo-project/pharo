*Morphic-Base
systemOn: aBuilder
	<worldMenu>
	(aBuilder item: #System)
		order: 3.0;
		iconName: #smallConfigurationIcon;
		with: [ (aBuilder item: #'About...')
				order: 0;
				iconName: #smallLanguageIcon;
				action: [ Smalltalk aboutThisSystem ].
			(aBuilder item: #'Start profiling all Processes')
				action: [ self startMessageTally ].
			(aBuilder item: #'Start profiling UI ')
				action: [ self startThenBrowseMessageTally ].
			(aBuilder item: #'Space left')
				action: [ Smalltalk informSpaceLeftAfterGarbageCollection ].
			(aBuilder item: #'Do Image Cleanup')
				action: [ Smalltalk cleanUp: true ];
				withSeparatorAfter.
			(aBuilder item: #'Start drawing again')
				action: [ World resumeAfterDrawError ].
			(aBuilder item: #'Start stepping again')
				action: [ World resumeAfterStepError ];
				withSeparatorAfter.
			(aBuilder item: #'Restore display (r)')
				action: [ World restoreMorphicDisplay ] ]