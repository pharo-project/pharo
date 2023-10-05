"
a simple trait used for NB callouts to cairo library functions
"
Trait {
	#name : 'TCairoLibrary',
	#category : 'Athens-Cairo-Library',
	#package : 'Athens-Cairo',
	#tag : 'Library'
}

{ #category : 'library path' }
TCairoLibrary classSide >> ffiLibraryName [
	^ CairoLibrary
]

{ #category : 'library path' }
TCairoLibrary >> ffiLibraryName [
	^ self class ffiLibraryName
]
