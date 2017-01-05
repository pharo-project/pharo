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
		"set rotating icon"
		self icon: self class refreshIcon1.
		rotating := [ | i |
			i := 1.
			[ true ] whileTrue:[ 
				i := (i \\ 10)+1.
				100 milliSeconds asDelay wait.
				self icon: (self class perform: ('refreshIcon', i asString) asSymbol) ] ] 
					forkAt: Processor activePriority - 1.
		"compute time consuming data"
		self morph: retrievingBlock value ]
	ensure: [
		rotating terminate.
		self icon: oldIcon.
		retrievingSemaphore signal ] ] forkAt: Processor activePriority -1