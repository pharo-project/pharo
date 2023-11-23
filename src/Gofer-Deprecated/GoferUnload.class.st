"
Unload
"
Class {
	#name : 'GoferUnload',
	#superclass : 'GoferWorking',
	#category : 'Gofer-Deprecated-Operations',
	#package : 'Gofer-Deprecated',
	#tag : 'Operations'
}

{ #category : 'private' }
GoferUnload >> defaultModel [

	^ (Smalltalk globals at: #MCMultiPackageLoader ifAbsent: [ MCPackageLoader ]) new
]

{ #category : 'running' }
GoferUnload >> execute [

	self workingCopies
		do: [ :copy | self unload: copy ].
	self model load.
	self gofer cleanup.
	self workingCopies
		do: [ :copy | self unregister: copy ]
]

{ #category : 'unloading' }
GoferUnload >> unload: aWorkingCopy [

	self unloadClasses: aWorkingCopy.
	self unloadPackage: aWorkingCopy
]

{ #category : 'unloading' }
GoferUnload >> unloadClasses: aWorkingCopy [

	aWorkingCopy systemPackage ifNotNil: [ :package |
		package methods do: [ :each | (each isClassSide and: [ each selector = #unload ]) ifTrue: [ each methodClass instanceSide unload ] ] ]
]

{ #category : 'unloading' }
GoferUnload >> unloadPackage: aWorkingCopy [

	self model unloadPackage: aWorkingCopy package
]

{ #category : 'unregistering' }
GoferUnload >> unregister: aWorkingCopy [

	self
		unregisterWorkingCopy: aWorkingCopy;
		unregisterRepositories: aWorkingCopy;
		unregisterPackageSet: aWorkingCopy;
		unregisterPackageInfo: aWorkingCopy
]

{ #category : 'unregistering' }
GoferUnload >> unregisterPackageInfo: aWorkingCopy [

	self packageOrganizer removePackage: aWorkingCopy packageName
]

{ #category : 'unregistering' }
GoferUnload >> unregisterPackageSet: aWorkingCopy [

	self packageOrganizer removePackage: aWorkingCopy packageName
]

{ #category : 'unregistering' }
GoferUnload >> unregisterRepositories: aWorkingCopy [

	aWorkingCopy repositoryGroup repositories allButFirst do: [ :repository |
		MCWorkingCopy allWorkingCopies do: [ :copy | (copy repositoryGroup includes: repository) ifTrue: [ ^ self ] ].
		MCRepositoryGroup default removeRepository: repository ]
]

{ #category : 'unregistering' }
GoferUnload >> unregisterWorkingCopy: aWorkingCopy [

	aWorkingCopy unregister
]
