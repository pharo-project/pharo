initialization
initialize
	super initialize.
	packages := MCWorkingCopy allManagers
		inject: #()
		into: [ :all :each | all, (each packageSet packages) ]