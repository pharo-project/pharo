"
Unload
"
Class {
	#name : #GoferUnload,
	#superclass : #GoferWorking,
	#category : #Gofer-Core
}

{ #category : #private }
GoferUnload >> defaultModel [
	^ (Smalltalk globals at: #MCMultiPackageLoader ifAbsent: [ MCPackageLoader ]) new
]

{ #category : #running }
GoferUnload >> execute [
	self workingCopies 
		do: [ :copy | self unload: copy ].
	self model load.
	self gofer cleanup.
	self workingCopies 
		do: [ :copy | self unregister: copy ]
]

{ #category : #unloading }
GoferUnload >> unload: aWorkingCopy [
	self unloadClasses: aWorkingCopy.
	self unloadPackage: aWorkingCopy

]

{ #category : #unloading }
GoferUnload >> unloadClasses: aWorkingCopy [
	aWorkingCopy packageSet methods do: [ :each |
		(each classIsMeta and: [ each selector = #unload ])
			ifTrue: [ each actualClass theNonMetaClass unload ] ]
]

{ #category : #unloading }
GoferUnload >> unloadPackage: aWorkingCopy [
	self model unloadPackage: aWorkingCopy package
]

{ #category : #unregistering }
GoferUnload >> unregister: aWorkingCopy [
	self unregisterWorkingCopy: aWorkingCopy.
	self unregisterRepositories: aWorkingCopy.
	self unregisterPackageSet: aWorkingCopy.
	self unregisterPackageInfo: aWorkingCopy
]

{ #category : #unregistering }
GoferUnload >> unregisterPackageInfo: aWorkingCopy [
	RPackageOrganizer default
		unregisterPackageNamed: aWorkingCopy packageName
]

{ #category : #unregistering }
GoferUnload >> unregisterPackageSet: aWorkingCopy [
	aWorkingCopy packageSet unregister
]

{ #category : #unregistering }
GoferUnload >> unregisterRepositories: aWorkingCopy [
	aWorkingCopy repositoryGroup repositories allButFirst do: [ :repository |
		MCWorkingCopy allManagers do: [ :copy |
			(copy repositoryGroup includes: repository)
				ifTrue: [ ^ self ] ].
		MCRepositoryGroup default
			removeRepository: repository ]
]

{ #category : #unregistering }
GoferUnload >> unregisterWorkingCopy: aWorkingCopy [
	aWorkingCopy unregister
]
