Extension { #name : #MCVersion }

{ #category : #'*Komitter-Models' }
MCVersion >> silentlyInitializeWithPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection [

	package := aPackage.
	info := aVersionInfo.
	snapshot := aSnapshot.
	dependencies := aCollection
]

{ #category : #'*Komitter-Models' }
MCVersion class >> silentlyPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection [

	^ self new silentlyInitializeWithPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection
]
