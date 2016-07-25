private - actions
elementaryReadServerUpdates
	"Scan the update server(s) for unassimilated updates. If the instance variable upToNumber is not nil, it represents the highest-numbered update to load.  This makes it possible to update only up to a particular point."

	"UpdateStreamer new elementaryReadServerUpdates"

	^ self elementaryReadServerUpdates: self newUpdates.