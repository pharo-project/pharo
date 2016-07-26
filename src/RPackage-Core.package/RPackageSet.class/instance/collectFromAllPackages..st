private
collectFromAllPackages: selector
	^self packages
		inject: #()
		into: [ :all :each | all, (each perform: selector) asArray ]