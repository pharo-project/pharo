Extension { #name : #MCGemstoneRepository }

{ #category : #'*Komitter-Models' }
MCGemstoneRepository >> koRemote [

	^ KomitGemstoneRemote new
		remote: self;
		yourself
]
