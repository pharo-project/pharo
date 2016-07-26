*GT-SpotterExtensions-Core
spotterPackageFor: aStep
	<spotterOrder: 21>
	self flag: #maybeRewriteForDirectStreaming.
	aStep listProcessor
		title: 'Packages';
		allCandidates: [ self package correspondingRPackage 
			ifNotNil: [ :rpackage | { rpackage } ] 
			ifNil: [ { } ] ];
		itemName: [ :item | item name ];
		itemIcon: [ Smalltalk ui icons iconNamed: #packageIcon ];
		filter: GTFilterSubstring