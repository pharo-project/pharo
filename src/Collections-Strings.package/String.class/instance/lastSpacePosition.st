testing
lastSpacePosition
	"Answer the character position of the final space or other separator character in the receiver, and 0 if none

	'fred the bear' lastSpacePosition
	'ziggie' lastSpacePosition
	'elvis ' lastSpacePosition
	'wimpy  ' lastSpacePosition
	'' lastSpacePosition
	"
	self size to: 1 by: -1 do:
		[:i | ((self at: i) isSeparator) ifTrue: [^ i]].
	^ 0
