pharo - updates
update60434
	"self new update60434"
	self withUpdateLog: '19801 Remove SublimishTheme
	https://pharo.fogbugz.com/f/cases/19801'.
	self loadTogether: self script60434 merge: false.
	
ScriptLoader new unloadPackageNamed: 'SublimishTheme'.
	self flushCaches.
