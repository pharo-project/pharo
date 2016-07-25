*Polymorph-Widgets
borderStyleToUse
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme scrollPaneNormalBorderStyleFor: self]
		ifFalse: [self theme scrollPaneDisabledBorderStyleFor: self]