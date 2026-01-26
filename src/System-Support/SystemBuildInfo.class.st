"
I am a class providing informations on the Pharo build. 

Currently I'm allowing one to know:
- if we are currently in the middle of a Pharo bootstrap or if it is done
- access the list of the system package

More info can be added later depending on needs
"
Class {
	#name : 'SystemBuildInfo',
	#superclass : 'Object',
	#instVars : [
		'systemPackagesNames'
	],
	#classVars : [
		'Current'
	],
	#category : 'System-Support-Image',
	#package : 'System-Support',
	#tag : 'Image'
}

{ #category : 'accessing' }
SystemBuildInfo class >> current [

	^ Current ifNil: [ Current := self new ]
]

{ #category : 'initialization' }
SystemBuildInfo >> initializeForRelease [

	systemPackagesNames := self packageOrganizer packageNames
]

{ #category : 'testing' }
SystemBuildInfo >> isBuildFinished [
	"We initialize the list of packages at the end so we can now if the build is finished when we have packages"

	^ systemPackagesNames isNotNil
]

{ #category : 'accessing' }
SystemBuildInfo >> systemPackageNames [

	^ systemPackagesNames
]

{ #category : 'accessing' }
SystemBuildInfo >> systemPackages [

	^ self packageOrganizer packages select: [ :package | self systemPackageNames includes: package name ]
]
