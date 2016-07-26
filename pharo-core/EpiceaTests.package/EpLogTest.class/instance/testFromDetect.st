tests
testFromDetect
	| reference isPrior entry |
	reference := log priorReferenceAt: log head.
	entry := log entryFor: reference.
	isPrior := false.
	" Assert for entry "
	self
		assert: (log from: reference detect: [ :dummy | true ])
		equals: entry.
	" Assert for the entry before 'entry' "
	self
		assert: (log
			from: reference
			detect: [ :dummy |
				isPrior
					ifFalse: [ isPrior := true. false ]
					ifTrue: [ true ] ])
		equals: (log entryFor: (log priorReferenceAt: entry)).