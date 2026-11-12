"
Recompiles all the methods of the selected packages.
"
Class {
	#name : 'SycRecompilePackageCommand',
	#superclass : 'SycPackageCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycRecompilePackageCommand >> canBeExecuted [

	^ context isAboutBrowserItemType: Package
]

{ #category : 'accessing' }
SycRecompilePackageCommand >> description [

	^ 'Recompiles all the methods of the selected packages.'
]

{ #category : 'execution' }
SycRecompilePackageCommand >> executeRefactoring [

	self packages do: [ :package | package methods do: [ :method | method recompile ] ] displayingProgress: [ :package | 'Recompiling ' , package name ]
]

{ #category : 'accessing' }
SycRecompilePackageCommand >> name [

	^ 'Recompile'
]
