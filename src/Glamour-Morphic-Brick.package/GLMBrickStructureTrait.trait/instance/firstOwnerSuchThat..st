brick-structure
firstOwnerSuchThat: conditionBlock

	self allOwnersDo: [:m | (conditionBlock cull: m) ifTrue: [^ m]].
	^ nil
