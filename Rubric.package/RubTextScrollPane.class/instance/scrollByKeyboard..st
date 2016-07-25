scrollbar managing
scrollByKeyboard: event
	"If event is ctrl+up/down then scroll and answer true"

	| ret sb |
	(event controlKeyPressed or: [ event commandKeyPressed ])
		ifFalse: [ ^ false ].
	sb := event commandKeyPressed
		ifTrue: [ self hScrollbar ]
		ifFalse: [ self vScrollbar ].
	self
		handleScrollerOffsetChange: [ 
			ret := false.
			event keyValue = 30
				ifTrue: [ 
					sb scrollUp: 3.
					ret := true ].
			event keyValue = 31
				ifTrue: [ 
					sb scrollDown: 3.
					ret := true ] ].
	^ ret