instance creation
contents: someContents behaviorName: behaviorName isMeta: isMeta category: categoryName stamp: stamp
	^self new
			contents: someContents;
			behaviorName: behaviorName;
			isMeta: isMeta;
			category: categoryName;
			stamp: stamp;
			yourself