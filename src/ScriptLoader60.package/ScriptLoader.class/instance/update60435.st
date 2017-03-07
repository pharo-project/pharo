pharo - updates
update60435
	"self new update60435"
	self withUpdateLog: '19816 Packages postfix must be removed
	https://pharo.fogbugz.com/f/cases/19816

19815 BaselineOfBasicTools still loads Polymorph-TaskbarIcons
	https://pharo.fogbugz.com/f/cases/19815

19807 Assert:equals: rule should target only tests
	https://pharo.fogbugz.com/f/cases/19807'.
	self loadTogether: self script60435 merge: false.
	self flushCaches.
