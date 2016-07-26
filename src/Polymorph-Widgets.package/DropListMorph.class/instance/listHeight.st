private
listHeight
	"Answer the height for the list."

	^(self listMorph listMorph height + 6 max: 38) min: (15 * self listFont height)