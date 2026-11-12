"
Files out the selected packages into a single chunk format file.
"
Class {
	#name : 'SycFileOutPackageCommand',
	#superclass : 'SycPackageCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycFileOutPackageCommand >> canBeExecuted [

	^ context isAboutBrowserItemType: Package
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> defaultFileName [

	^ self packages size = 1
		  ifTrue: [ self packages first name ]
		  ifFalse: [ 'packagefileout' ]
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> description [

	^ 'Files out the selected packages into a single chunk format file.'
]

{ #category : 'execution' }
SycFileOutPackageCommand >> executeRefactoring [

	Fileouter fileoutNamed: self defaultFileName configureWith: [ :fileouter | self packages do: [ :package | fileouter addFullPackage: package ] ]
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> iconName [

	^ #save
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> name [

	^ 'File out'
]
