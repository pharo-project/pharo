"
Files out the selected packages into a single chunk format file.
"
Class {
	#name : 'SycFileOutPackageCommand',
	#superclass : 'SycPackageExtraCmCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'accessing' }
SycFileOutPackageCommand >> defaultFileName [

	^ self packages size = 1
		  ifTrue: [ self packages first name ]
		  ifFalse: [ 'packagefileout' ]
]

{ #category : 'execution' }
SycFileOutPackageCommand >> executeRefactoring [

	Fileouter fileoutNamed: self defaultFileName configureWith: [ :fileouter | self packages do: [ :package | fileouter addFullPackage: package ] ]
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> icon [

	^ self iconNamed: #save
]

{ #category : 'testing' }
SycFileOutPackageCommand >> isApplicable [

	^ context isAboutBrowserItemType: Package
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> name [

	^ 'File out'
]
