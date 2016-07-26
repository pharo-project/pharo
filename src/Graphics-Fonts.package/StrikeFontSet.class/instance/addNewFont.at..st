private
addNewFont: aFont at: encodingIndex

	| newArray |
	encodingIndex > fontArray size ifTrue: [
		newArray := Array new: encodingIndex.
		newArray replaceFrom: 1 to: fontArray size with: fontArray startingAt: 1.
	] ifFalse: [
		newArray := fontArray.
	].

	newArray at: encodingIndex put: aFont.

	self initializeWithFontArray: newArray.
