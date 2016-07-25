actions
viewHistory
	workingCopy ifNotNil:
		[(MCWorkingHistoryBrowser new
				ancestry: workingCopy ancestry;
				package: workingCopy package)
			label:  'Version History: ',  workingCopy packageName;
			show]