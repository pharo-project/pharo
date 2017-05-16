pharo - updates
update60490
	"self new update60490"
	self withUpdateLog: '20040 integrate iceberg
	https://pharo.fogbugz.com/f/cases/20040'.
	self loadTogether: self script60490 merge: false.
	
BaselineOfIDE new loadIceberg.
	self flushCaches.
