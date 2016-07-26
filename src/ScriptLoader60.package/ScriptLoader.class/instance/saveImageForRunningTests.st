menu integration actions
saveImageForRunningTests

	| comment commentQuoted st resuming |
	resuming := false.
	Smalltalk image saveSession
		ifFalse: [ resuming := true].
	resuming ifTrue: [
		self saveAsImageAsTestImage.
	
		self inform: 'Now you can use this image named ''Test'' to run the tests and come back to fix problems in the \ previous one and eventually freeze it by selecting the next step, in either one.' withCRs]