accessing
soundTheme
	"Answer the sound theme to use, if not specified
	use the default sound theme."

	^soundTheme ifNil: [self defaultSoundTheme]