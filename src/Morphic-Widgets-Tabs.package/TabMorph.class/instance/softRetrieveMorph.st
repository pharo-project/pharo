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
	retrievingProcess := [ [
		rotating := [ | i newIcon |
			i := 1.
			20 milliSeconds asDelay wait.
			UIManager default defer: [self icon: self class refreshIcon1].
			[ true ] whileTrue:[ 
				i := (i \\ 10)+1.
				100 milliSeconds asDelay wait.
				newIcon := (self class perform: ('refreshIcon', i asString) asSymbol).
				UIManager default defer: [
					self icon: newIcon ] ]] 
					forkAt: Processor activePriority + 1.
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