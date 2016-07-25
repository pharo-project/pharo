scrollbar-updating
updateStyle

	self bar color: self scrollBarColor.
	self layout floating: self scrollbarPosition.
	self width: self scrollbarWidth.
	
	self layout floating = #right ifTrue: [ self marginRight: self scrollbarOffset ].
	self layout floating = #bottom ifTrue: [ self marginBottom: self scrollbarOffset ].
	self layout floating = #left ifTrue: [ self marginLeft: self scrollbarOffset ].
	self layout floating = #top ifTrue: [ self marginTop: self scrollbarOffset ]