instance creation
newWithBinaryFileStream
	^ (FLPlatform current isPharo and: [
		FLPlatform current versionNumber >= 5 ])
			ifTrue: [
				FLBinaryFileStreamStrategy basicNew
					initialize;
					yourself ]
			ifFalse: [ self newWithStandardFileStream ]