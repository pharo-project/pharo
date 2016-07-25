private
detectContentDispositionValue: aString
	self contentDispositionValues do: [ :each | 
		((each beginsWith: aString)
			and: [ each size > (aString size + 1)
			and: [ (each at: aString size + 1) = $= ] ]) ifTrue: [
				^ (each copyAfter: $=) withoutQuoting ] ].
	^ nil