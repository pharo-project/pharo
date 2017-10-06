protocol
softRetrieveMorph
	| oldIcon |
	"Like retrieveMorph but done in a separated process"	
	morph ifNotNil: [ ^ self ].	
	retrievingProcess 
		ifNotNil: [ 
			"The retrieving process is already running, let's wait for its result"
			retrievingProcess isTerminated 
				ifFalse: [ ^ self ] ].
	
	oldIcon := self icon ifNotNil: [ :i | i form ].
	label icon: TabBuildingIconMorph new.
	retrievingProcess := [ [
		morph := retrievingBlock value]
	ensure: [		
		UIManager default defer: [
			self icon: oldIcon.
			self triggerEvent: #tabRefreshed with: self].
		"I should signal all waiters here"
		[retrievingSemaphore isEmpty] whileFalse: [ retrievingSemaphore signal].
	] ] forkAt: Processor activePriority - 2