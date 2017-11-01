Extension { #name : #KomitNewSlice }

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
