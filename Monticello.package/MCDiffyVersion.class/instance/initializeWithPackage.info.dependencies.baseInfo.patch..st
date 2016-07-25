initialize-release
initializeWithPackage: aPackage info: aVersionInfo dependencies: aCollection baseInfo: baseVersionInfo patch: aPatch
	patch := aPatch.
	base := baseVersionInfo.
	super initializeWithPackage: aPackage info: aVersionInfo snapshot: nil dependencies: aCollection.
