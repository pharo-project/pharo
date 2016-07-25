optimizing
removeEmptyStart

	 (startSequence size = 1 and: [ startSequence last isPushClosureCopy not ]) ifTrue: [
        "startSeq is just unconditional jump, forget it"
        startSequence := startSequence last destination].
