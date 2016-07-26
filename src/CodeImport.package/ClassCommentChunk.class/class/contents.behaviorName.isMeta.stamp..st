instance creation
contents: someContents behaviorName: behaviorName isMeta: aBoolean stamp: stamp
	^self new
			contents: someContents;
			behaviorName: behaviorName;
			isMeta: aBoolean;
			stamp: stamp;
			yourself