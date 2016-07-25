*Polymorph-Widgets
roundedCorners
	"Return a list of those corners to round.

		1-4
		|  |
		2-3

	Returned array contains `codes' of those corners, which should be rounded.

	1 denotes top-left corner
	2 denotes bottom-left corner
	3 denotes bottom-right corner
	4 denotes top-right corner.

	Thus, if this method returned #(2 3) that would mean that bottom (left and right)
	corners would be rounded whereas top (left and right) corners wouldn't be rounded.

	This method returns #(1 2 3 4) and that means that all the corners should be rounded."

	^self valueOfProperty: #roundedCorners ifAbsent: [#(1 2 3 4)]