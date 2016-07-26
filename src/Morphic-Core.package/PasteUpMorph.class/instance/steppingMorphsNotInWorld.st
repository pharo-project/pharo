project state
steppingMorphsNotInWorld
	| all |
	all := self allMorphs.
	^ self listOfSteppingMorphs reject: [ :m | all includes: m ]	"self currentWorld steppingMorphsNotInWorld do: [:m | m delete]"