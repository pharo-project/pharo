"
Root of the Extras commands of the package browser menus. The packages are the selected packages of the context unless set explicitly.
"
Class {
	#name : 'SycPackageExtraCmCommand',
	#superclass : 'SycCmCommand',
	#instVars : [
		'packages'
	],
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'accessing' }
SycPackageExtraCmCommand class >> activationStrategy [

	^ SycExtraMenuActivation
]

{ #category : 'accessing' }
SycPackageExtraCmCommand >> packages [

	^ packages ifNil: [ context selectedPackages ]
]

{ #category : 'accessing' }
SycPackageExtraCmCommand >> packages: anObject [

	packages := anObject
]
