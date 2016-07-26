focus
focusedMorphAt: pageIndex
	"returns previosely focused morph in the pane or nil if no morph was focused"
	
	^ focusedMorphs at: pageIndex ifAbsent: [ nil ]