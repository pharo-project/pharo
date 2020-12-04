Extension { #name : #MicMetaDataBlock }

{ #category : #'*Microdown-Pillar' }
MicMetaDataBlock >> asPillar [
	
	^ PRMetadata new 
			metadata: (STON fromString: '{', self body, '}'); yourself
	

]
