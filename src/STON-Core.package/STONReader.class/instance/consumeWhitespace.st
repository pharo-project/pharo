private
consumeWhitespace
	"Strip whitespaces from the input stream."

	[ readStream atEnd not and: [ readStream peek isSeparator ] ]
		whileTrue: [ readStream next ]