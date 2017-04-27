pharo - updates
update60469
	"self new update60469"
	self withUpdateLog: '19960 Comments in changed code in Diff tool
	https://pharo.fogbugz.com/f/cases/19960

19962 Watery theme is broken because SimpleButtonMorph does not understand isDefault
	https://pharo.fogbugz.com/f/cases/19962

19970 Formatting methods recently introduced for WelcomeHelp should be accessible to all Help classes
	https://pharo.fogbugz.com/f/cases/19970'.
	self loadTogether: self script60469 merge: false.
	
Pharo3DarkTheme  beCurrent.
	self flushCaches.
