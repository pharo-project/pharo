pharo - updates
update60465
	"self new update60465"
	self withUpdateLog: '19932 WelcomeHelp for Pharo 6 update
	https://pharo.fogbugz.com/f/cases/19932

19934 change default theme to DarkTheme
	https://pharo.fogbugz.com/f/cases/19934

19933 Remove ConfigurationOfMooseAlgos from the image.
	https://pharo.fogbugz.com/f/cases/19933

19929 Comment guard to warn debugging top most process context
	https://pharo.fogbugz.com/f/cases/19929

19926 Glamour Fast Table selection menu item actions does not work together with a filter
	https://pharo.fogbugz.com/f/cases/19926'.
	self loadTogether: self script60465 merge: false.
	
Pharo3DarkTheme beCurrent.
ScriptLoader new unloadPackageNamed: #ConfigurationOfMooseAlgos.
	self flushCaches.
