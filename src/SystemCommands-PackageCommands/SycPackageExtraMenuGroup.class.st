"
I am the ""Extras"" sub-menu of the context menu of packages and package tags.
"
Class {
	#name : 'SycPackageExtraMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'accessing' }
SycPackageExtraMenuGroup >> iconName [

	^ #smallRedo
]

{ #category : 'accessing' }
SycPackageExtraMenuGroup >> name [

	^ 'Extras'
]

{ #category : 'accessing' }
SycPackageExtraMenuGroup >> order [

	^ 10200
]
