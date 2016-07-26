accessing
current
	"Current := nil"
	Current ifNil: [
		Current := self determineCurrentLocale.
		"Transcript show: 'Current locale: ' , Current localeID asString; cr"].
	^Current