accessing
borderStyleToUse
	"Answer the borderStyle that should be used for the receiver."
	
	^self isSelected
		ifTrue: [self theme tabLabelSelectedBorderStyleFor: self]
		ifFalse: [self theme tabLabelNormalBorderStyleFor: self]