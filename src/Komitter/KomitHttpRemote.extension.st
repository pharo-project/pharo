Extension { #name : #KomitHttpRemote }

{ #category : #'*Komitter-UI' }
KomitHttpRemote >> icon [

	^ 'HTTP'
]

{ #category : #'*Komitter-UI' }
KomitHttpRemote >> label [
	
	^ self remote location
]
