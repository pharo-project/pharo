multilingual scanning
widthOf: char inFont: aFont

	(char isMemberOf: CombinedChar) ifTrue: [
		^ aFont widthOf: char base.
	] ifFalse: [
		^ aFont widthOf: char.
	].


