structure
outermostMorphThat: conditionBlock
	"Return the outermost containing morph for which aBlock is true, or nil if none"

	| outermost |
	self allOwnersDo: [:m | (conditionBlock value: m) ifTrue: [outermost := m]].
	^ outermost