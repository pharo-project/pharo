*GT-SpotterExtensions-Core
spotterSuperClassMethodsFor: aStep
	<spotterOrder: 16>
	|superclasses|
	superclasses := self class withAllSuperclasses select: #isMeta.
	aStep listProcessor
			title: 'Super class methods';
			allCandidates: [ self classSide allMethods select: [ :each |(each methodClass = self classSide) not & (superclasses includes: each methodClass) ] ];
			filter: GTFilterSubstring