test-generation
generateAllTests
	EpCodeChange allSubclassesDo: [ :class |
		| sourceStream |
		sourceStream := String new writeStream.
		(class includesSelector: #accept:) ifTrue: [ 
			self class 
				compile:
				(sourceStream << 'test' << (class name allButFirst: 2) << Character cr <<
				'	self testItem: ' << class name << ' new.') contents
				classified: 'tests'
		].
	].