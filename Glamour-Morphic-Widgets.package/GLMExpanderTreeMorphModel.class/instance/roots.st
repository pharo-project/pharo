accessing
roots
"	| filtered |"
	roots ifNil: [ self roots: self glamourExpander panes ].
	^ roots
"	filtered := roots select: [ :each | each shouldBeDisplayed ].
	((self shouldFilterByAmount and: [
		self amountToFilterBy < filtered size]) and: [
			filtered notEmpty]) ifTrue: [
		filtered := filtered first: self amountToFilterBy ].
	^ filtered"