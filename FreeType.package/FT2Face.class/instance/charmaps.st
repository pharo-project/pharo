accessing
charmaps
	"Answer an Array of Strings naming the different character maps available for setCharMap:"
	charmaps ifNil: [
		charmaps := Array new: numCharmaps.
		self getCharMapsInto: charmaps ].
	^charmaps