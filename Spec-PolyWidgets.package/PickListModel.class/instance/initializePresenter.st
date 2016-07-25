initialization
initializePresenter
	labelClickableHolder
		whenChangedDo:
			[ :aBoolean | associationsWrapperToItem keysDo: [ :e | e labelClickable: aBoolean ] ].
	defaultValueHolder
		whenChangedDo: [ :value | associationsWrapperToItem keysDo: [ :cb | cb state: value ] ]