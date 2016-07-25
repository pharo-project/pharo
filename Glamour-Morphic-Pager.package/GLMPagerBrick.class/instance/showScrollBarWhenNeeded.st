pager-accessing
showScrollBarWhenNeeded
	"retuns true if scrollbar should be visible if number of pages > 1,
	otherwise false"
	^ showScrollBarWhenNeeded ifNil: [ showScrollBarWhenNeeded := true ]