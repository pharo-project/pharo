updating
updateTopHeader
	self topHeader ifNotNil: [:th | | w |
		th width: scroller width.
		w := self headerBounds.
		th submorphsDo: [:sm | w ifNotEmpty: [sm bounds: w removeFirst; layoutInset: self columnInset @ 0]]].
