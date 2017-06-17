accessing ui
remoteActionsFor: aRemote
	^ self plugins 
		inject: #() 
		into: [ :all :each | all, (each remoteActionsFor: aRemote) ]