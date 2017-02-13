pharo - updates
update60387
	"self new update60387"
	self withUpdateLog: '19689 Add method comments to cool block messages
	https://pharo.fogbugz.com/f/cases/19689

19687 Improve UUID documentation, implementation and test coverage
	https://pharo.fogbugz.com/f/cases/19687

19691 TabMorph background building animation should be in more priority than building process 
	https://pharo.fogbugz.com/f/cases/19691

19690 Help on Help includes examples that are not executable
	https://pharo.fogbugz.com/f/cases/19690'.
	self loadTogether: self script60387 merge: false.
	self flushCaches.
