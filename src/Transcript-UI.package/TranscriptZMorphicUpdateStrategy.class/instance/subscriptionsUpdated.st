updating
subscriptionsUpdated

	"perhaps one could support multiple worlds by doing:
	 	((aTranscript subscriptions select: [ :each | each isMorph]) 
			collect: [ :morph | morph world ] as: Set) 
				do: [ :world | world ifNotNil: [ world displayWorldSafely ] ]
	"
	World displayWorldSafely. 
	
	nextUpdate := Time millisecondClockValue + updateDelay