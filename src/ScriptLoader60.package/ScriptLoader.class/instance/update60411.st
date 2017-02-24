pharo - updates
update60411
	"self new update60411"
	self withUpdateLog: '19747 ClassRename refactoring is not working anymore
	https://pharo.fogbugz.com/f/cases/19747

19750 catalog does not create meta repository for Pharo 50 
	https://pharo.fogbugz.com/f/cases/19750

19751 Rename Copy to Clipboard to Copy Stack to Clipboard
	https://pharo.fogbugz.com/f/cases/19751'.
	self loadTogether: self script60411 merge: false.
	self flushCaches.
