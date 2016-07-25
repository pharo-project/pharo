private
radioBorderStyleToUse
	"Answer the borderStyle that should be used for the receiver when it is a radio button."
	
	^self selected
		ifTrue: [self enabled
			ifTrue: [self theme radioButtonSelectedBorderStyleFor: self]
			ifFalse: [self theme radioButtonSelectedDisabledBorderStyleFor: self]]
		ifFalse: [self enabled
			ifTrue: [self theme radioButtonNormalBorderStyleFor: self]
			ifFalse: [self theme radioButtonDisabledBorderStyleFor: self]]