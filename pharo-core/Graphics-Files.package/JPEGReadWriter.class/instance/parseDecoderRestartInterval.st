marker parsing
parseDecoderRestartInterval
	| length |
	length := self nextWord.
	length = 4 ifFalse: [ self error: 'DRI length incorrect' ].
	restartInterval := self nextWord