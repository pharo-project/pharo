private
checkboxFillStyleToUse
	"Answer the fillStyle that should be used for the receiver when it is a checkbox."
	
	^self selected
		ifTrue: [self enabled
			ifTrue: [self theme checkboxButtonSelectedFillStyleFor: self]
			ifFalse: [self theme checkboxButtonSelectedDisabledFillStyleFor: self]]
		ifFalse: [self enabled
			ifTrue: [self theme checkboxButtonNormalFillStyleFor: self]
			ifFalse: [self theme checkboxButtonDisabledFillStyleFor: self]]