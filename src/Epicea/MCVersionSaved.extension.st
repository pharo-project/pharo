Extension { #name : #MCVersionSaved }

{ #category : #'*Epicea' }
MCVersionSaved >> asEpiceaEvent [
	
	^ EpMonticelloVersionSave version: version repository: repository
]
