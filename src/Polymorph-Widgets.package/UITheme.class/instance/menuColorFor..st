fill-styles
menuColorFor: aThemedMorph
	"Answer the menu color to use."

	|w|
	^self menuColor ifNil: [
		aThemedMorph ifNil: [^self defaultMenuColor].
		w := aThemedMorph isSystemWindow
			ifTrue: [aThemedMorph]
			ifFalse: [aThemedMorph ownerThatIsA: SystemWindow].
		w isNil
			ifTrue: [self defaultMenuColor]
			ifFalse: [(w valueOfProperty: #paneColor) ifNil: [self defaultMenuColor]]]