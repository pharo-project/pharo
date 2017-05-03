pharo - updates
update60480
	"self new update60480"
	self withUpdateLog: '19986 STON do not support WideSymbols
	https://pharo.fogbugz.com/f/cases/19986

20013 WelcomeHelp for Pharo 6
	https://pharo.fogbugz.com/f/cases/20013

19979 Revert gray comments color and fix colors in DiffMorph
	https://pharo.fogbugz.com/f/cases/19979'.
	self loadTogether: self script60480 merge: false.
	
Pharo3DarkTheme beCurrent.
	self flushCaches.
