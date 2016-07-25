running
setUp

	super setUp.
	saveCurrent := Locale current.
	Locale localeAnnouncer when: LocaleChanged send: #localeWasChanged: to: self