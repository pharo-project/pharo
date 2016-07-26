accessing scrollbars
scrollerOffsetChanged
	super scrollerOffsetChanged.
	self announcer
		announce:
			(PaneScrolling new
				step: self scrollPane hScrollbarValue @ self scrollPane vScrollbarValue;
				scrollPane: self scrollPane;
				yourself)