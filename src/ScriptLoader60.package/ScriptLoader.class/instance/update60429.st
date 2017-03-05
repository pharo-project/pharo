pharo - updates
update60429
	"self new update60429"
	self withUpdateLog: '19793 SHTextStylerST80 always parser code as method if workspace is nil
	https://pharo.fogbugz.com/f/cases/19793

19797 Add access methods in PharoCommonTools instead of using DNU
	https://pharo.fogbugz.com/f/cases/19797'.
	self loadTogether: self script60429 merge: false.
	self flushCaches.
