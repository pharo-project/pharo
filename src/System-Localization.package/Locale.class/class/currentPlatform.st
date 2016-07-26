accessing
currentPlatform
	"CurrentPlatform := nil"
	CurrentPlatform ifNil: [CurrentPlatform := self determineCurrentLocale].
	^CurrentPlatform