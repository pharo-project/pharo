tests
testDefaultUpdatesListFileName
	"Usually 'updatesXX.list' where XX is the Pharo version like 40"

	|streamer|
	streamer := UpdateStreamer new.
	self 
		assert: (streamer updatesListFileName beginsWith: 'updates');
		assert: (streamer updatesListFileName endsWith: '.list')