as yet unclassified
testInstallFromStream
	| stream |
	stream := RWBinaryOrTextStream on: String new.
	MCMczWriter fileOut: expected on: stream.
	MczInstaller installStream: stream reset.
	self assertNoChange.
	self assertVersionInfoPresent.
	