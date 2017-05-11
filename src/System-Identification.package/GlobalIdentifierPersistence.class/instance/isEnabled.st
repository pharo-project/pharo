testing
isEnabled
	"If HOME directory does not exists, ignore the persistence mechanism.
	HOME directory may not exists in case of a server deployment.
	See https://pharo.fogbugz.com/f/cases/19944"
	^ self checker isEnabled