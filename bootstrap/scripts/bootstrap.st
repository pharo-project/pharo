[
| options architecture |

options := CommandLineArguments default.
architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].

"Configure logging"
(CustomStringStreamBeacon with: FileStream stdout)
	startFor: PBStepStartedSignal, PBStepFinishedSignal.

FileStream stdout
	nextPutAll: ('[INFO] Building Image Arch ', architecture);
	cr.


(PBBootstrap forArchitecture: architecture)
	prepareBootstrap;
	createImage

] on: Warning do: #resume
