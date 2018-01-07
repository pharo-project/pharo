Extension { #name : #RG2Environment }

{ #category : #'*Ring2-Monticello' }
RG2Environment >> writeIntoFileTree: aMCFileTreeRepository [

	(self ask packages select: #isRing2Resolved) do: [ :aPackage |
		| aSnapshot info version |
		aSnapshot := aPackage asMCSnapshot.

		info := MCVersionInfo new.
		info instVarNamed: #name put: aPackage name, '-author'.
		version := MCVersion package: (MCPackage new name: aPackage name) info: info snapshot: aSnapshot dependencies: #().
		IceMetadatalessFileTreeWriter
			fileOut: (version)
			on: (aMCFileTreeRepository) ].
		
]
