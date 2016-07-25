updating
update: aSymbol with: anObject
	"Handle tab changes."

	super update: aSymbol.
	aSymbol == #selectedIndex
		ifTrue: [ |selectedPage|
				selectedPage := self tabGroup pages at: anObject ifAbsent: [nil].
				selectedPage ifNotNil: [
						selectedPage rememberKeyboardFocus: ActiveHand keyboardFocus.
						self tabGroup page ifNotNil: [self tabGroup page activate].]
				]