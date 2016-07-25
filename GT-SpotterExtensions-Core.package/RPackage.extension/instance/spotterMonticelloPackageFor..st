*GT-SpotterExtensions-Core
spotterMonticelloPackageFor: aStep
	<spotterOrder: 41>
	aStep listProcessor
		title: 'Monticello Package';
		allCandidates: [ self mcPackage 
			ifNotNil: [ :mcPackage | Array with: mcPackage ] 
			ifNil: [ #() ] ];
		itemIcon: [ Smalltalk ui icons iconNamed: #monticelloPackageIcon ];
		filter: GTFilterSubstring