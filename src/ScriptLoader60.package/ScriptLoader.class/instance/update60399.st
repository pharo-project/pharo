pharo - updates
update60399
	"self new update60399"
	self withUpdateLog: '19721 fix #name send in Slot tests
	https://pharo.fogbugz.com/f/cases/19721'.
	self loadTogether: self script60399 merge: false.
	self flushCaches.
