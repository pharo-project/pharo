"
I am a dummy object used to commit code in a new slice
"
Class {
	#name : #KomitNewSlice,
	#superclass : #KomitRemote,
	#category : 'Komitter-Models'
}

{ #category : #'*Komitter-UI' }
KomitNewSlice >> icon [
	^ self iconNamed: #komitterNewSlice
]

{ #category : #'*Komitter-UI' }
KomitNewSlice >> komitOn: aKomitterUI [

	^ aKomitterUI newSliceFor: self
]

{ #category : #'*Komitter-UI' }
KomitNewSlice >> label [

	^ 'New Slice'
]

{ #category : #accessing }
KomitNewSlice >> remote [
	
	^ MCRepositoryGroup default repositories 
		detect: [ :e | e description = 'http://smalltalkhub.com/mc/Pharo/Pharo50Inbox/main/' ]
		ifNone: [ MCRepositoryGroup default repositories detect: [ :e | e description = 'http://smalltalkhub.com/mc/Pharo/Pharo50Inbox/main' ] ]
]
