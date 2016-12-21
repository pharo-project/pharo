pharo - updates
update60334
	"self new update60334"
	self withUpdateLog: '19479 Typo in name attribute of GRVariableAssignedLiteralRule
	https://pharo.fogbugz.com/f/cases/19479

19478 Criticsbrowser shows a refactoring class name instead of the text diff
	https://pharo.fogbugz.com/f/cases/19478

19438 Widen tolerance for DelayScheduler timing tests
	https://pharo.fogbugz.com/f/cases/19438'.
	self loadTogether: self script60334 merge: false.
	self flushCaches.
