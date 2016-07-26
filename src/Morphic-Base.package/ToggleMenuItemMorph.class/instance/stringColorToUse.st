accessing
stringColorToUse
	"Answer the state dependent color to use for drawing text."
	
	^self isEnabled
		ifTrue: [self isSelected
					ifTrue: [self theme menuItemSelectedTextColorFor: self]
					ifFalse: [self theme menuItemNormalTextColorFor: self]]
		ifFalse: [self theme menuItemDisabledTextColorFor: self]