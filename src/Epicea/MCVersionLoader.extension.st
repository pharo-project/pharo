Extension { #name : #MCVersionLoader }

{ #category : #'*Epicea-Triggering' }
MCVersionLoader >> asEpiceaEvent [

	^ EpMonticelloVersionsLoad versions: versions
]

{ #category : #'*Epicea-Triggering' }
MCVersionLoader >> isEpiceaInterestingJobOwner [

	^ true
]
