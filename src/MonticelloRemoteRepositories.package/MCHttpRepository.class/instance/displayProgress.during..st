i/o
displayProgress: label during: workBlock
	| nextUpdateTime |
	nextUpdateTime := 0.
	^UIManager default displayProgress: label 
		from: 0.0 to: 1.0 during:[:bar|
			[workBlock value] on: HTTPProgress do:[:ex|
				(ex total == nil or: [ex amount == nil]) ifFalse:[
					(nextUpdateTime < Time millisecondClockValue 
						or:[ex total = ex amount]) ifTrue:[
							bar current: ex amount asFloat / ex total asFloat.
							nextUpdateTime := Time millisecondClockValue + 100.
					].
				].
				ex resume.
			]
		].
