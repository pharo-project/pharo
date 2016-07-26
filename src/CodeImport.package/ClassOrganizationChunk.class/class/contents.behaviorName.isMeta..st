instance creation
contents: someContents behaviorName: behaviorName isMeta: aBoolean
	^self new
		contents: someContents;
		behaviorName: behaviorName;
		isMeta: aBoolean;
		yourself