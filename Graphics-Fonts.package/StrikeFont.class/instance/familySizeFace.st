accessing
familySizeFace
	"Answer an array with familyName, a String, pointSize, an Integer, and
	faceCode, an Integer."

	^Array with: name
		with: self height
		with: emphasis

	"(1 to: 12) collect: [:x | (TextStyle default fontAt: x) familySizeFace]"