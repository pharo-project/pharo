menu actions
deleteCloseBox
	closeBox ifNotNil:
		[closeBox delete.
		closeBox := nil]