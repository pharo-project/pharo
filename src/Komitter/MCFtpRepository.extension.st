Extension { #name : #MCFtpRepository }

{ #category : #'*Komitter-Models' }
MCFtpRepository >> isRemote [
	^ true
]

{ #category : #'*Komitter-Models' }
MCFtpRepository >> koRemote [
	
	^ KomitFtpRemote new
		remote: self;
		yourself
]
