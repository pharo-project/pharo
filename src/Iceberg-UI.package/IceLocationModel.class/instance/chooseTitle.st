accessing
chooseTitle
	^ chooseTitle ifNil: [ self class defaultChooseDialogTitle ]