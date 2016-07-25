accessing
selectionBarColor
	^ selectionBarColor ifNil: [selectionBarColor := self selectionColor alpha: 0.06]