factory
buildWidget
	^ self autoRefresh
		ifTrue: [ self buildWidgetAutoRefresh ]
		ifFalse: [ self buildWidgetNoRefersh ]