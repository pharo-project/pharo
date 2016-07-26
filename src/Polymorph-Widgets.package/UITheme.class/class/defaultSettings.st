accessing
defaultSettings
	"Answer the default settings for the theme."
	
	^defaultSettings ifNil: [
		self defaultSettings: self newDefaultSettings.
		defaultSettings]