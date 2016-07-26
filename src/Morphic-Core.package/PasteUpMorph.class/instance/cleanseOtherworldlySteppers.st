stepping
cleanseOtherworldlySteppers
	"If the current project is a morphic one, then remove from its steplist
	those morphs that are not really in the world"
	"Utilities cleanseOtherworldlySteppers"
	| old delta |
	old := self currentWorld stepListSize.
	self currentWorld steppingMorphsNotInWorld
		do: [:m | m delete].
	self currentWorld cleanseStepList.
	(delta := old - self currentWorld stepListSize) > 0
		ifTrue: [ self crTrace: delta asString , ' morphs removed from steplist' ]