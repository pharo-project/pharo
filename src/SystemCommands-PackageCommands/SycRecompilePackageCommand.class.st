Class {
	#name : #SycRecompilePackageCommand,
	#superclass : #SycPackageCommand,
	#category : #'SystemCommands-PackageCommands'
}

{ #category : #accessing }
SycRecompilePackageCommand >> defaultMenuItemName [
	^'Recompile'
]

{ #category : #execution }
SycRecompilePackageCommand >> execute [
	packages 
		do: [ :eachPackage | 
			eachPackage methods do: [ :eachMethod | eachMethod recompile ] ]
		displayingProgress: [:eachPackage | 'Recompiling ', eachPackage name]
]
