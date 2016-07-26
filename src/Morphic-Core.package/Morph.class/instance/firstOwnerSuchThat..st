structure
firstOwnerSuchThat: conditionBlock

	self allOwnersDo: [:m | (conditionBlock value: m) ifTrue: [^ m]].
	^ nil
