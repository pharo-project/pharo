report
getNewTabsFor: tabs
	^ tabs size < self maxTabs
		ifTrue: [ tabs ]
		ifFalse: [ (tabs select: [ :x | x = '[' ]) copyWith: '[' ]