accessing
projectSpec
	^ baselineProjectSpec
		ifNil: [ 
			self assert: configurationProjectSpec notNil.
			configurationProjectSpec ]