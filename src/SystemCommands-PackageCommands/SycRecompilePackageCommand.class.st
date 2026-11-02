Class {
	#name : 'SycRecompilePackageCommand',
	#superclass : 'SycPackageExtraCmCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'execution' }
SycRecompilePackageCommand >> executeRefactoring [
	packages
		do: [ :eachPackage |
			eachPackage methods do: [ :eachMethod | eachMethod recompile ] ]
		displayingProgress: [:eachPackage | 'Recompiling ', eachPackage name]
]

{ #category : 'accessing' }
SycRecompilePackageCommand >> name [
	^'Recompile'
]
