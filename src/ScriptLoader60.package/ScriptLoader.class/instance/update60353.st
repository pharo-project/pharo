pharo - updates
update60353
	"self new update60353"
	self withUpdateLog: '19574 update BaselineOfIDE
	https://pharo.fogbugz.com/f/cases/19574'.
	self loadTogether: self script60353 merge: false.
	self flushCaches.
