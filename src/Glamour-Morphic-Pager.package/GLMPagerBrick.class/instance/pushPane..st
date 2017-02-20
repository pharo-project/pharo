pager-api
pushPane: aMorph
	"pushes a pane to the end with smooth scroll animation"
	| isSmooth |
	
	isSmooth := self pagerModel size >= self pagerModel numberOfVisiblePages.
	self pagerModel pushPane: aMorph smooth: isSmooth