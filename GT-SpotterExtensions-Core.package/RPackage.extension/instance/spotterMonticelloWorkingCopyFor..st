*GT-SpotterExtensions-Core
spotterMonticelloWorkingCopyFor: aStep
	<spotterOrder: 42>
	aStep listProcessor
		title: 'Monticello Working Copy';
		allCandidates: [ self mcPackage 
			ifNotNil: [ :mcPackage | Array with: mcPackage workingCopy ] 
			ifNil: [ #() ] ];
		itemIcon: [ Smalltalk ui icons iconNamed: #versionControlIcon ];
		filter: GTFilterSubstring