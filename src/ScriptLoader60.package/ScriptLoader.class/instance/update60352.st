pharo - updates
update60352
	"self new update60352"
	self withUpdateLog: '19554 #~~ should be a primitive and a compile-time inlined selector to be consistent with #==
	https://pharo.fogbugz.com/f/cases/19554

19573 more rigid ThemeIcons defaultUrl
	https://pharo.fogbugz.com/f/cases/19573'.
	self loadTogether: self script60352 merge: false.
	self flushCaches.
