initialization
initialize

	super initialize.

	toolbar := OkCancelToolbar new asValueHolder.
	toolbar whenChangedDo: [ :t | self changed: #toolbar: with: { t } ]