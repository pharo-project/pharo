accessing ui
packageActionsFor: aPackage
	^ self plugins
		inject: #() 
		into: [ :all :each | all, (each packageActionsFor: aPackage) ]