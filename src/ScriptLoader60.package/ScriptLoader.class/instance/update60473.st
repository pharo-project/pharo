pharo - updates
update60473
	"self new update60473"
	self withUpdateLog: '19968 Class rename do not mark extending packages dirty
	https://pharo.fogbugz.com/f/cases/19968

19987 Bad highlight for matching parentheses with dark theme
	https://pharo.fogbugz.com/f/cases/19987

19995 Fix typo in testIsNativeSSLPluginPresent
	https://pharo.fogbugz.com/f/cases/19995

19996 Cleanup FFIConstants
	https://pharo.fogbugz.com/f/cases/19996'.
	self loadTogether: self script60473 merge: false.
	
MCWorkingCopy registerForNotifications.
	self flushCaches.
