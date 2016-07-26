initialization
initializeWithName: vName id: aUUID message: aString date: aDate time: aTime author: initials ancestors: aCollection stepChildren: stepCollection
	name := vName.
	id := aUUID.
	message := aString.
	date := aDate ifNil: [ Date today ].
	time := aTime.
	author := initials.
	ancestors := aCollection.
	stepChildren := stepCollection