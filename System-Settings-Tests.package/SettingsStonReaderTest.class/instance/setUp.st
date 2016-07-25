running
setUp
	super setUp.
	reader := SettingsStonReader new.
	reader stream: (ReadStream on: String new)
