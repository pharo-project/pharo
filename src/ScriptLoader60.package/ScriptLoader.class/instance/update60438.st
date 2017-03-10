pharo - updates
update60438
	"self new update60438"
	self withUpdateLog: '19786 MonticelloFileTree-Core package exported to Git contains a strange extension
	https://pharo.fogbugz.com/f/cases/19786

19825 circular dependency between Renraku and Refactoring-Critics
	https://pharo.fogbugz.com/f/cases/19825

19824 fix #scrollBarDownButtonBoundsFor: calculation
	https://pharo.fogbugz.com/f/cases/19824

19819 Epicea browser doesnt refresh when tests produce code changes
	https://pharo.fogbugz.com/f/cases/19819'.
	self loadTogether: self script60438 merge: false.
	self flushCaches.
