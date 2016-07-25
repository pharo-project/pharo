tests
testDefaultServer
	|streamer|
	streamer := UpdateStreamer new.	
	self assert: streamer server = streamer defaultServer