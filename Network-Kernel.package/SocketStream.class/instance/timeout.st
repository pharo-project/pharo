configuration
timeout
	"Lazily initialized unless it has been set explicitly."

	timeout ifNil: [timeout := Socket standardTimeout].
	^timeout