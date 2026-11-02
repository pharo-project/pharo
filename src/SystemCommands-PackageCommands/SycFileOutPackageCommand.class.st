"
Requests to save a textual version of the selected package to a new file in chunk format and .st extension. 
"
Class {
	#name : 'SycFileOutPackageCommand',
	#superclass : 'SycPackageExtraCmCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycFileOutPackageCommand >> canBeExecuted [
	^ context isAboutBrowserItemType: Package
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> defaultFileName [
	^ packages size = 1
		ifTrue: [ packages first name ]
		ifFalse: [ 'packagefileout' ]
]

{ #category : 'execution' }
SycFileOutPackageCommand >> executeRefactoring [

	^ Fileouter fileoutNamed: self defaultFileName configureWith: [ :fileouter | packages do: [ :package | fileouter addFullPackage: package ] ]
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> icon [
	^ self iconNamed: #save
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> name [

	^ 'File out'
]
