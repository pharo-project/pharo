fill-styles
menuTitleColorFor: aThemedMorph
	"Answer the menu titlecolor to use."

	|w|
	^self menuTitleColor ifNil: [
		aThemedMorph ifNil: [^self defaultMenuTitleColor].
		w := aThemedMorph isSystemWindow
			ifTrue: [aThemedMorph]
			ifFalse: [aThemedMorph ownerThatIsA: SystemWindow].
		w isNil
			ifTrue: [self defaultMenuTitleColor]
			ifFalse: [(w valueOfProperty: #paneColor) darker ifNil: [self defaultMenuTitleColor]]]