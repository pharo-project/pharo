"
I represent the specifics of running Unix in a ARM32 processor
"
Class {
	#name : 'UnixARM32Platform',
	#superclass : 'Unix32Platform',
	#category : 'System-Platforms-Unix',
	#package : 'System-Platforms',
	#tag : 'Unix'
}

{ #category : 'testing' }
UnixARM32Platform class >> isActivePlatform [

	^ (Smalltalk vm operatingSystemName = 'unix')
		and: [ Smalltalk vm is32bit
			and: [ Smalltalk vm architectureName = 'armv7l' ] ]
]
