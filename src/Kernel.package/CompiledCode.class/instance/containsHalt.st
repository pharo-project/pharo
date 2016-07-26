testing
containsHalt

	^ self sendsAnySelectorOf: #( halt halt: haltIf: haltIfNil haltOnCount: haltOnce)