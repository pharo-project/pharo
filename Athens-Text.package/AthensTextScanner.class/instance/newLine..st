as yet unclassified
newLine: startingIndex 

	currentLine := lines add: (AthensTextLine new text: text;
		startIndex: startingIndex; yourself).

"	currentStrip ifNotNil: [
		
		currentLine addCommand: currentFont copy.
		currentLine addCommand: currentColor copy.

		currentBackground ifNotNil: [
			currentBackground := currentBackground copy start: 0; width: 0. 
			currentLine addCommand: currentBackground ].
	].
"
"reset everything, to make sure all attributes will be reapplied for new line"
	
	currentColor := currentBackground := currentFont := currentStrip := nil.