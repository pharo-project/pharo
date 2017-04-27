pharo - updates
update60470
	"self new update60470"
	self withUpdateLog: '19958 Find and Replace dialog uses black text color in dark theme
	https://pharo.fogbugz.com/f/cases/19958

19963 Improvement of TravisIntegrationHelp
	https://pharo.fogbugz.com/f/cases/19963

19977 Menu failure on right click in code pane when coming from MessageBrowser
	https://pharo.fogbugz.com/f/cases/19977'.
	self loadTogether: self script60470 merge: false.
	self flushCaches.
