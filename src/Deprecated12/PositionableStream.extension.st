Extension { #name : #PositionableStream }

{ #category : #'*Deprecated12' }
PositionableStream >> untilEnd: aBlock displayingProgress: aString [
	self deprecated: 'This method will be removed in Pharo 13. You can inline the code if you need it.'.
	aString
		displayProgressFrom: 0 to: self size
		during:
			[:bar |
			[self atEnd] whileFalse:
				[bar current: self position.
				aBlock value]]
]

{ #category : #'*Deprecated12' }
PositionableStream >> untilEndWithFork: aBlock displayingProgress: aString [
	| sem done result |
	self deprecated: 'This method will be removed in Pharo 13. You can inline the code if you need it.'.
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
]
