accessing
switchTo: locale 
	"Locale switchTo: Locale isoLanguage: 'de'"
	Current localeID = locale localeID
		ifFalse: [
			|announcement|
			announcement := LocaleChanged from: Current to: locale.
			Current := locale.
			CurrentPlatform := locale.
			self localeAnnouncer announce: announcement ]