pharo - updates
update60333
	"self new update60333"
	self withUpdateLog: '19484 BaselineOfMorphic references unexisting class
	https://pharo.fogbugz.com/f/cases/19484

19483 Add bootstrap specific code in Monticello
	https://pharo.fogbugz.com/f/cases/19483'.
	self loadTogether: self script60333 merge: false.
	self flushCaches.
