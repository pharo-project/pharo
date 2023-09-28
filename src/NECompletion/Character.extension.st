Extension { #name : 'Character' }

{ #category : '*NECompletion' }
Character >> isCompletionCharacter [
	"Method stays for readability of user"
	^ self tokenish
]
