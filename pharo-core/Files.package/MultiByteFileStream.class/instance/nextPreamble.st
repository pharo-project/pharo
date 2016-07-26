fileIn/Out
nextPreamble
	"Assuming that preamble part does not contain ]lang[ tag"
	self skipSeparators.
	^ self basicChunk
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
								ifNotNil: [converter restoreStateOf: self with: state.]]]