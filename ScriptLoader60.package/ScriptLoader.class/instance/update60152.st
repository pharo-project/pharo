pharo - updates
update60152
	"self new update60152"
	self withUpdateLog: '18774 Fix some critics
	https://pharo.fogbugz.com/f/cases/18774

18746 fix some typo in method comments
	https://pharo.fogbugz.com/f/cases/18746

18745 Move what are not widget from Spec-core-widget to another subpackage of Spec-core
	https://pharo.fogbugz.com/f/cases/18745

'.
	self loadTogether: self script60152 merge: false.
	self flushCaches.
self cleanRepositories.
