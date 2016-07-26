accessing
storedSettings
	"self storedSettings"
	| builder |
	builder := StoredSettingsFactory new.
	^ self nodeSettings collect: [ :e | builder from: e ]