brick-tab-label-accessing
selected: aBoolean
	"Use the appropriate label."

	self isSelected
		ifTrue: [ self beSelected ]
		ifFalse: [ self beNormal ]