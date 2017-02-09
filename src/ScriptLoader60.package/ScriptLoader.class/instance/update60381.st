pharo - updates
update60381
	"self new update60381"
	self withUpdateLog: '19673 Use NeoUUIDGenerator for UUID generation
	https://pharo.fogbugz.com/f/cases/19673

19672 Cleanup some (Neo)UUIDGenerator dependencies
	https://pharo.fogbugz.com/f/cases/19672'.
	self loadTogether: self script60381 merge: false.
	self flushCaches.
