fileIn/Out
nextChunk
	"Answer the contents of the receiver, up to the next terminator
	character. Doubled terminators indicate an embedded terminator
	character."
	self skipSeparators.
	^ self
		parseLangTagFor: (self basicChunk
				ifNil: [String
						new: 1000
						streamContents: [:stream | 
							| character state |
							[(character := self next) == nil
								or: [character == $!
										and: [state := converter saveStateOf: self..
											self next ~~ $!]]]
								whileFalse: [stream nextPut: character].
							character
								ifNotNil: [converter restoreStateOf: self with: state.]]])