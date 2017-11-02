Class {
	#name : #KomitFtpRemote,
	#superclass : #KomitRemote,
	#category : 'Komitter-Models'
}

{ #category : #'*Komitter-UI' }
KomitFtpRemote >> icon [

	^ 'FTP'
]

{ #category : #'*Komitter-UI' }
KomitFtpRemote >> label [
	
	^ self remote location
]
