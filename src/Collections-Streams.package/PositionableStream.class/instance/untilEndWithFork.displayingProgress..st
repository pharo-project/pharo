positioning
untilEndWithFork: aBlock displayingProgress: aString
	| sem done result |
	sem := Semaphore new.
	done := false.
	[ 
	result := [ aBlock value ]
		ensure: [ 
			done := true.
			sem signal ] ] fork.
	self
		untilEnd: [ 
			done
				ifTrue: [ ^ result ].
			(Delay forSeconds: 0.2) wait ]
		displayingProgress: aString.
	sem wait.
	^ result