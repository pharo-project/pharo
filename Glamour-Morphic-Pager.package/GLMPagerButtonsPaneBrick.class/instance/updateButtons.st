buttons-updating
updateButtons
	"first we uncheck all buttons"
	self buttons select: #isFocused thenDo: #uncheck.
	"then check only necessary"
	self pagerModel size > 0 ifTrue: [
		self buttons from: self pagerModel firstVisiblePageIndex to: self pagerModel lastVisiblePageIndex do: #check ]