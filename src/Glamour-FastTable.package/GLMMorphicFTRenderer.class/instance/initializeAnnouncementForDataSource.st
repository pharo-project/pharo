private
initializeAnnouncementForDataSource
	tableModel announcer
		when: GLMTreeMorphSelectionChanged
		do: [ :ann | 
			tableModel glamourPresentation announcer
				suspendAllWhile: [ 
					tableModel glamourPresentation selection: ann selectionValue.
					tableModel glamourPresentation selectionPath: ann selectionPathValue ] ].
	tableModel announcer
		when: GLMTreeMorphStrongSelectionChanged
		do: [ :ann | tableModel glamourPresentation strongSelection: ann strongSelectionValue ].
	tableMorph
		onAnnouncement: FTStrongSelectionChanged send: #strongSelection: to: tableModel;
		onAnnouncement: FTSelectionChanged send: #selection: to: tableModel.
	self installActionsOnModel: tableModel fromPresentation: tableModel glamourPresentation.
	self installKeystrokeActionsOnMorph: tableMorph fromPresentation: tableModel glamourPresentation.
