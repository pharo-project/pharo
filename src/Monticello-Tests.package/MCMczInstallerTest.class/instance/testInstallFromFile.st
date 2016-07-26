as yet unclassified
testInstallFromFile
	MCMczWriter fileOut: expected on: self fileStream.
	MczInstaller installFileNamed: self fileName.
	self assertNoChange.