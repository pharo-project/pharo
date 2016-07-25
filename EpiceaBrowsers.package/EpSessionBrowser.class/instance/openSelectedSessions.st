private
openSelectedSessions

	sessionsTreeModel selectedItems do: [ :item |
		(EpLog freshFromFile: item content asFileReference) browseEvents ]