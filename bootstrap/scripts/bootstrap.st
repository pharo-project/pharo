[options := CommandLineArguments default.
architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].

('[INFO] Building Image Arch ', architecture) logCr.

"Configure logging"
(CustomStringStreamBeacon with: FileStream stdout)
	startFor: PBStepStartedSignal, PBStepFinishedSignal.

(CustomStringStreamBeacon with: 'bootstrap.log' asFileReference writeStream)
	startFor: PBStepStartedSignal, PBStepFinishedSignal, PBSubStepFinishedSignal.

(PBBootstrap forArchitecture: architecture)
	prepareBootstrap;
	createImage

] on: Warning do: #resume
