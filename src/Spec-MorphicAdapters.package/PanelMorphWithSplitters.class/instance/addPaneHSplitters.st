private
addPaneHSplitters
	"Add the horizontal pane splitters."

	|remaining targetBottom sameBottom sameTop|
	remaining := paneMorphs reject: [:each |
			each layoutFrame bottomFraction = 1 or: [
				each layoutFrame bottomFraction = 0]].
	[remaining notEmpty] whileTrue: [
		targetBottom := remaining first layoutFrame bottomFraction.
		sameBottom := remaining select: [:each |
			each layoutFrame bottomFraction = targetBottom].
		sameTop := paneMorphs select: [:each |
			each layoutFrame topFraction = targetBottom].
		remaining := remaining
			copyWithoutAll: (self addPaneHSplitterBetween: sameBottom and: sameTop)]