buttons-updating
updateButtonsFocus
	"unfocus all"
	self buttons do: #setNotFocused.
	"because it's possible when nothing has focus"
	self pagerModel focusedPageIndex
		ifNotNil: [ (self buttons at: self pagerModel focusedPageIndex) setFocused ]
	