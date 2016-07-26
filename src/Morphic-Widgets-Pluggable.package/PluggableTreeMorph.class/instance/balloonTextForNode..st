node access
balloonTextForNode: node
	getHelpSelector ifNil:[^nil].
	^model perform: getHelpSelector with: node item