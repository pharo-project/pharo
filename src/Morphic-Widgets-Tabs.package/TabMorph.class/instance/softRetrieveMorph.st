protocol
softRetrieveMorph
	| rotating oldIcon |
	"Like retrieveMorph but done in a separated process"	
	morph ifNotNil: [ ^ self ].	
	retrievingProcess 
		ifNotNil: [ 
			"The retrieving process is already running, let's wait for its result"
			retrievingProcess isTerminated 
				ifFalse: [ ^ self ] ].
	
	oldIcon := self icon ifNotNil: [ :i | i form ].
	rotating := self createAnimationProcess.
	retrievingProcess := [ [
		rotating resume.
		"compute time consuming data"
		morph := retrievingBlock value.
		retrievingSemaphore signal.
		UIManager default defer: [
			self triggerEvent: #tabRefreshed with: self.
			self icon: oldIcon]]
	ensure: [
		rotating terminate.
		UIManager default defer: [self icon: oldIcon]
	] ] forkAt: Processor activePriority - 2