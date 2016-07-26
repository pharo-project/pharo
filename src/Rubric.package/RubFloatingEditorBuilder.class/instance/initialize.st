initialization
initialize
	super initialize.
	announcer := self defaultAnnouncer.
	initialContents := ''.
	acceptOnCR := true.
	autoAccept := false.
	escapeBlock := [  ].
	font := StandardFonts defaultFont.
	customizeValuable := [  ]