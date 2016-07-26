comparing
= aShortcut
	(aShortcut isKindOf: self class)
		ifFalse: [^ false].
	^ (aShortcut modifiers includesAll: modifiers) and: [ modifiers includesAll: aShortcut modifiers ]