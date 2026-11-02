"
Recompiles all the methods of the selected packages.
"
Class {
	#name : 'SycRecompilePackageCommand',
	#superclass : 'SycPackageExtraCmCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'execution' }
SycRecompilePackageCommand >> executeRefactoring [

	self packages do: [ :package | package methods do: [ :method | method recompile ] ] displayingProgress: [ :package | 'Recompiling ' , package name ]
]

{ #category : 'testing' }
SycRecompilePackageCommand >> isApplicable [

	^ context isAboutBrowserItemType: Package
]

{ #category : 'accessing' }
SycRecompilePackageCommand >> name [

	^ 'Recompile'
]
