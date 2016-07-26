writing
fileName
	"Answer a filename to serialize and materialize using it."

	^ FLPlatform current absolutePathOf: (FLPlatform current imageName, '-tests.fuel')