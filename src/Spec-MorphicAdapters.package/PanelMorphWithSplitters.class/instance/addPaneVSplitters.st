private
addPaneVSplitters
	"Add the vertical pane splitters."

	|remaining targetRight sameRight sameLeft |
	remaining := paneMorphs reject: [:each |
			each layoutFrame rightFraction = 1 or: [
				each layoutFrame rightFraction = 0]].
	[remaining notEmpty] whileTrue: [
		targetRight := remaining first layoutFrame rightFraction.
		sameRight := remaining select: [:each |
			each layoutFrame rightFraction = targetRight].
		sameLeft := paneMorphs select: [:each |
			each layoutFrame leftFraction = targetRight and: [
				each layoutFrame rightFraction ~= targetRight]].
		remaining := remaining
			copyWithoutAll: (self addPaneVSplitterBetween: sameRight and: sameLeft)]