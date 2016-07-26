menu spec
collectConfigurationMenuSpecs
	| menuSpecs | 
	menuSpecs := (Pragma allNamed: #'contextMenu:' in: self class)
		select: [ :each | (each argumentAt: 1) = #configuration ].
		
	^ menuSpecs 
		inject: #()
		into: [ :all :each | all, (each method valueWithReceiver: self arguments: #()) ]
	