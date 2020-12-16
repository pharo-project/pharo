Extension { #name : #RGEnvironment }

{ #category : #'*Ring-Monticello' }
RGEnvironment >> writeIntoFileTree: aMCFileTreeRepository [

	(self ask packages select: #isRingResolved) do: [ :aPackage |
		| aSnapshot info aVersion |
		aSnapshot := aPackage asMCSnapshot.

		info := MCVersionInfo new.
		info instVarNamed: #name put: aPackage name, '-author'.
		aVersion := MCVersion package: (MCPackage new name: aPackage name) info: info snapshot: aSnapshot dependencies: #().
		IceMetadatalessFileTreeWriter
			fileOut: (aVersion)
			on: (aMCFileTreeRepository) ].
		
]

{ #category : #'*Ring-Monticello' }
RGEnvironment >> writeIntoTonel: aFileReference [
	(self ask packages select: [ :package | package isRingResolved and: package isEmpty not ]) do: [ :aPackage |
		| aSnapshot info aVersion |
		aSnapshot := aPackage asMCSnapshot.

		info := MCVersionInfo new.
		info instVarNamed: #name put: aPackage name, '-author'.
		aVersion := MCVersion package: (MCPackage new name: aPackage name) info: info snapshot: aSnapshot dependencies: #().
		TonelWriter 
			fileOut: (aVersion)
			on: aFileReference ]		
]
