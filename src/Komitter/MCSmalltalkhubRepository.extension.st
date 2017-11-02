Extension { #name : #MCSmalltalkhubRepository }

{ #category : #'*Komitter-Models' }
MCSmalltalkhubRepository >> koRemote [

	^ KomitSmalltalkhubRemote new
		remote: self;
		yourself
]
