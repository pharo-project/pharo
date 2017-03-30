pharo - updates
update60455
	"self new update60455"
	self withUpdateLog: '19882 copyBits fails when showing Athens surface
	https://pharo.fogbugz.com/f/cases/19882

19885 STON cannot read classes with underscore in their name
	https://pharo.fogbugz.com/f/cases/19885

19879 Accessors for instance and class variable rename refactoring
	https://pharo.fogbugz.com/f/cases/19879

19881 add Time >> #hour:minute:second:nano: constructor
	https://pharo.fogbugz.com/f/cases/19881'.
	self loadTogether: self script60455 merge: false.
	
Smalltalk recreateSpecialObjectsArray.
	self flushCaches.
