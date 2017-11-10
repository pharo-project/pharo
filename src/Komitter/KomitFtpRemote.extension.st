Extension { #name : #KomitFtpRemote }

{ #category : #'*Komitter-UI' }
KomitFtpRemote >> icon [

	^ 'FTP'
]

{ #category : #'*Komitter-UI' }
KomitFtpRemote >> label [
	
	^ self remote location
]
