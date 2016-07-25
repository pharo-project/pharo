api
title
	| wpName |
	wpName := self watchpoint tagname.
	^'WP: ',(wpName isAllSeparators ifTrue: [ 'Unnamed' ] ifFalse: [ wpName ]).