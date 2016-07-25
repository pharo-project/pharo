private
checkboxBorderStyleToUse
	"Answer the borderStyle that should be used for the receiver when it is a checkbox."
	
	^self selected
		ifTrue: [self enabled
			ifTrue: [self theme checkboxButtonSelectedBorderStyleFor: self]
			ifFalse: [self theme checkboxButtonSelectedDisabledBorderStyleFor: self]]
		ifFalse: [self enabled
			ifTrue: [self theme checkboxButtonNormalBorderStyleFor: self]
			ifFalse: [self theme checkboxButtonDisabledBorderStyleFor: self]]