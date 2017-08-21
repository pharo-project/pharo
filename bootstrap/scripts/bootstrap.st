[
| options architecture |

options := CommandLineArguments default.
architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].
buildNumber := options optionAt: 'BUILD_NUMBER' ifAbsent: [ 'NO_CI' ].

"Configure logging"
(CustomStringStreamBeacon with: FileStream stdout)
	startFor: PBStepStartedSignal, PBStepFinishedSignal.

FileStream stdout
	nextPutAll: ('[INFO] Building Image Arch ', architecture);
	cr.


(PBBootstrap forArchitecture: architecture)
	buildNumber: buildNumber;
	prepareBootstrap;
	createImage

] on: Warning do: #resume
