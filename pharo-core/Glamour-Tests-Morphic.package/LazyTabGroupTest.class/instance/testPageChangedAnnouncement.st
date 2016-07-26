tests
testPageChangedAnnouncement
	| announcement page |
	announcement := 0.
	tabs announcer
		when: LazyTabPageChanged
		do: [ :ann | 
			announcement := announcement + 1.
			page := ann page ].
	tabs selectedPageIndex: 1.
	self assert: announcement equals: 1.
	self assert: (page isKindOf: LazyTabPage)