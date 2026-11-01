Class {
	#name : 'SycRecompilePackageCommand',
	#superclass : 'SycPackageCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'execution' }
SycRecompilePackageCommand >> execute [
	packages
		do: [ :eachPackage |
			eachPackage methods do: [ :eachMethod | eachMethod recompile ] ]
		displayingProgress: [:eachPackage | 'Recompiling ', eachPackage name]
]

{ #category : 'accessing' }
SycRecompilePackageCommand >> name [
	^'Recompile'
]
