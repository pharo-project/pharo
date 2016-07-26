private
collectFromAllPackages: selector with: anObject
	^self packages
		inject: #()
		into: [ :all :each | all, (each perform: selector with: anObject) asArray ]