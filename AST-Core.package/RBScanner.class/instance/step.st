private
step
	stream atEnd 
		ifTrue: 
			[characterType := #eof.
			^currentCharacter := nil].
	currentCharacter := stream next.
	characterType := self classify: currentCharacter.
	^currentCharacter