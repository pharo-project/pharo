private
setSuccessor: newSuccessor

	successor := newSuccessor.
	paragraph ifNotNil: [paragraph wantsColumnBreaks: successor notNil].
