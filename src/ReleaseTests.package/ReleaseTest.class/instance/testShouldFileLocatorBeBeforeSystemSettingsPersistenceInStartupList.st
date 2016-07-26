testing
testShouldFileLocatorBeBeforeSystemSettingsPersistenceInStartupList
	"This test documents issue https://pharo.fogbugz.com/f/cases/17721/Starting-Image-50617-raises-MNU"
	| startupList settingsIndex |
	
	startupList := (SessionManager default startupList collect: #handledId).
	
	settingsIndex := startupList indexOf: #SystemSettingsPersistence.
	settingsIndex > 0
		ifTrue: [ self should: [ (startupList indexOf: #FileLocator) < (startupList indexOf: #SystemSettingsPersistence) ] ]