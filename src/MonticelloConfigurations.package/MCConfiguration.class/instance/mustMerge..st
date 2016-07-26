private
mustMerge: aVersion
	"answer true if we have to do a full merge and false if we can simply load instead"
	
	| pkg wc current |
	(pkg := aVersion package) hasWorkingCopy ifFalse: [^false "no wc -> load"].
	(wc := pkg workingCopy) modified ifTrue: [^true "modified -> merge"].
	wc ancestors isEmpty ifTrue: [^true "no ancestor info -> merge"].
	current := wc ancestors first.
	(aVersion info hasAncestor: current) ifTrue: [^false "direct descendant of wc -> load"].
	"new branch -> merge"
	^true