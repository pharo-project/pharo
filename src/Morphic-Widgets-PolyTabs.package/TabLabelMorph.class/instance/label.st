accessing
label
	"Answer the label contents."

	^self isSelected
		ifTrue: [self selectedLabel userString ifNil: [self firstSubmorph]]
		ifFalse: [self normalLabel userString ifNil: [self firstSubmorph]]