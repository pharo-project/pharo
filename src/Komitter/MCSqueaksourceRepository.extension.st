Extension { #name : #MCSqueaksourceRepository }

{ #category : #'*Komitter-Models' }
MCSqueaksourceRepository >> koRemote [

	^ KomitSqueakSourceRemote new
		remote: self;
		yourself
]
