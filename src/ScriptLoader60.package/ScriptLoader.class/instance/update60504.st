pharo - updates
update60504
	"self new update60504"
	self withUpdateLog: '20185 Request pipe after block argments
	https://pharo.fogbugz.com/f/cases/20185

20183 TraitDescription>>fileOutLocalMethodsInCategory:on: method temp var name overlap
	https://pharo.fogbugz.com/f/cases/20183

20184 Request space between argument and selector in StdioStream>>next:
	https://pharo.fogbugz.com/f/cases/20184'.
	self loadTogether: self script60504 merge: false.
	self flushCaches.
