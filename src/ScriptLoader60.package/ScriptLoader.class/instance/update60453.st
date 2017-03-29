pharo - updates
update60453
	"self new update60453"
	self withUpdateLog: '19873 Breakpoint method icon is fail when breakpoint set on ast node
	https://pharo.fogbugz.com/f/cases/19873

19871 TabMorph could fail if it removed too fast while content is buiding in background
	https://pharo.fogbugz.com/f/cases/19871

19876 Improve FuzzyMatcher
	https://pharo.fogbugz.com/f/cases/19876'.
	self loadTogether: self script60453 merge: false.
	self flushCaches.
