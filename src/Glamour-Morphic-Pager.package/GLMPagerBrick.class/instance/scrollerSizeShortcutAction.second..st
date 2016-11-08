pager-actions
scrollerSizeShortcutAction: first second: second
	
	(first = #left) & (second = #left) ifTrue: [ pagerModel enlargeToLeft ].
	(first = #left) & (second = #right) ifTrue: [ pagerModel diminishToRight ].
	(first = #right) & (second = #left) ifTrue: [ pagerModel diminishToLeft ].
	(first = #right) & (second = #right) ifTrue: [ pagerModel enlargeToRight ]