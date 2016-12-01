pharo - updates
update60315
	"self new update60315"
	self withUpdateLog: '19411 improve comment RBGlobalNode for class variable
	https://pharo.fogbugz.com/f/cases/19411

19410 improve comment  OCRequestorScope
	https://pharo.fogbugz.com/f/cases/19410

19400 argumentNames returns orderedCollection
	https://pharo.fogbugz.com/f/cases/19400'.
	self loadTogether: self script60315 merge: false.
	
Smalltalk recompile.
	self flushCaches.
