Extension { #name : 'OSKeyboardEvent' }

{ #category : '*OSWindow-Morphic' }
OSKeyboardEvent >> asKeyCombination [
	| modifier control command shift alt char |

	control := self modifiers ctrl.
	command := self modifiers cmd.
	shift := self modifiers shift.
	alt := self modifiers alt.
	char := self modifiedCharacter.

	(shift | command | control | alt)
		ifFalse: [^ KMSingleKeyCombination from: char ].

	modifier := KMNoShortcut new.
	control ifTrue: [ modifier := modifier + KMModifier ctrl ].
	command ifTrue: [ modifier := modifier + KMModifier command ].
	shift ifTrue: [ modifier := modifier + KMModifier shift ].
	alt ifTrue: [ modifier := modifier + KMModifier alt ].

	^ modifier + char
]
