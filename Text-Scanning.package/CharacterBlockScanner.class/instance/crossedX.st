stop conditions
crossedX
	"Text display has wrapping. The scanner just found a character past the x 
	location of the cursor. We know that the cursor is pointing at a character 
	or before one."

	self retrieveLastCharacterWidth.
	
	characterPoint x <= (destX + (lastCharacterWidth // 2))
		ifTrue:	[characterPoint := destX @ destY.
				^true].
	lastIndex >= line last 
		ifTrue:	[characterPoint := destX @ destY.
				^true].

	"Pointing past middle of a character, return the next character."
	lastIndex := lastIndex + 1.
	characterPoint := destX + lastCharacterWidth + kern @ destY.
	^ true