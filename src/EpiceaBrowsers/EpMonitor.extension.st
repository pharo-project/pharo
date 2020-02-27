Extension { #name : #EpMonitor }

{ #category : #'*EpiceaBrowsers' }
EpMonitor >> statusIcon [
	^ self isEnabled
		ifTrue: [ #testGreen ]
		ifFalse: [ #testNotRun ]
]

{ #category : #'*EpiceaBrowsers' }
EpMonitor >> statusString [
	^ self isEnabled
		ifTrue: [ 'Monitor enabled' ]
		ifFalse: [ 'Monitor disabled' ]
]
