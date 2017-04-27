pharo - updates
update60468
	"self new update60468"
	self withUpdateLog: '19956 switching back to pharo3 theme has wrong tooltip color
	https://pharo.fogbugz.com/f/cases/19956

19937 Help for TravisIntegration
	https://pharo.fogbugz.com/f/cases/19937

19961 Impossible to read popup error in playground and Dark Theme
	https://pharo.fogbugz.com/f/cases/19961'.
	self loadTogether: self script60468 merge: false.
	self flushCaches.
