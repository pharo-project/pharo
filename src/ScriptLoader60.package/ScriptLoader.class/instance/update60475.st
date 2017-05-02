pharo - updates
update60475
	"self new update60475"
	self withUpdateLog: '19993 Provide comment for TAbleToRotate
	https://pharo.fogbugz.com/f/cases/19993

19959 base local Metaclass and ClassTrait methods tests on pragmas 
	https://pharo.fogbugz.com/f/cases/19959

19989 testAllCallsOnASymbol can fail easily
	https://pharo.fogbugz.com/f/cases/19989

19997 Cleanup the Help menu (order)
	https://pharo.fogbugz.com/f/cases/19997'.
	self loadTogether: self script60475 merge: false.
	self flushCaches.
