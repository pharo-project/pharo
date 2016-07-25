tests
testPageRemovedAnnouncement
	| announcement |
	announcement := 0.
	tabs announcer when: LazyTabPageRemoved do: [ :ann | announcement := announcement + 1 ].
	tabs announcer when: LazyTabPageChanged do: [ :ann | announcement := announcement + 1 ].
	tabs removePage: tabs pages first.
	self assert: announcement equals: 1