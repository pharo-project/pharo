accessing
like: aCharacter
	"Answer aCharacter if aCharacter is in the receiver, or nil if it is absent."

	^ (self includes: aCharacter) ifTrue: [ aCharacter ] ifFalse: [ nil ]