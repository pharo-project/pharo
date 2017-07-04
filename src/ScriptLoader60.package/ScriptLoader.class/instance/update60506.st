pharo - updates
update60506
	"self new update60506"
	self withUpdateLog: '20092 Improve speed of WriteStream>>tab:
	https://pharo.fogbugz.com/f/cases/20092

20088 Improve speed of Character>>isSeparator in when the separator is not a space
	https://pharo.fogbugz.com/f/cases/20088

20204 Text class>>#fromString
	https://pharo.fogbugz.com/f/cases/20204'.
	self loadTogether: self script60506 merge: false.
	self flushCaches.
