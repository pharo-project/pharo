running
setUp
	super setUp.
	writer := SettingsStonWriter new.
	writer stream: (WriteStream on: String new).
