as yet unclassified
updateSource
	"Update the source difference morph."

	|sel|
	sel := self selectedChangeWrapper.
	self diffMorph allowJoinClicks: (sel notNil and: [
		sel isConflict and: [sel operation isModification]]).
	super updateSource.
	(sel isNil or: [sel isConflict not]) ifTrue: [^self].
	sel localChosen
		ifTrue: [self diffMorph indicateSrc]
		ifFalse: [self diffMorph indicateDst]