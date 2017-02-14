pharo - updates
update60391
	"self new update60391"
	self withUpdateLog: '19703 add empty ShortRunArray to Deprecated60
	https://pharo.fogbugz.com/f/cases/19703'.
	self loadTogether: self script60391 merge: false.
	self flushCaches.
