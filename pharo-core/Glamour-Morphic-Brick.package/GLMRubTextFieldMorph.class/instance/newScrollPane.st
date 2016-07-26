initialize-release
newScrollPane
	"using hacked ScrollPane to remove focus border"
	| sp |
	sp := GLMRubTextScrollPane new.
	sp scrollTarget: self defaultScrollTarget.
	^ sp