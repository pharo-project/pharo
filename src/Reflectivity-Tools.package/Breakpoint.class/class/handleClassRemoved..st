system announcements
handleClassRemoved: anAnnouncement
	AllBreakpoints copy do: [ :breakpoint |
		breakpoint link methods
			detect: [ :m | m methodClass = anAnnouncement classRemoved ]
			ifFound: [ AllBreakpoints remove: breakpoint ] ]