private
radioFillStyleToUse
	"Answer the fillStyle that should be used for the receiver when it is a radio button."
	
	^self selected
		ifTrue: [self enabled
			ifTrue: [self theme radioButtonSelectedFillStyleFor: self]
			ifFalse: [self theme radioButtonSelectedDisabledFillStyleFor: self]]
		ifFalse: [self enabled
			ifTrue: [self theme radioButtonNormalFillStyleFor: self]
			ifFalse: [self theme radioButtonDisabledFillStyleFor: self]]