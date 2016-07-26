accessing
splitAt: position
	| newSpan |
	"split receiver on two character spans "
	" position points to first character which should be included in newly created span "
	
	position == start ifTrue: [ ^ self ].
	(position < start or: [ position > stop ]) ifTrue: [ self error: 'invalid position' ].

	newSpan := self class new
		start: position;
		stop: stop.

	stop := position - 1.
	self insert: newSpan.