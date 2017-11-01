Class {
	#name : #MetacelloTestsGoferUnload,
	#superclass : #GoferUnload,
	#category : 'Metacello-TestsMC'
}

{ #category : #running }
MetacelloTestsGoferUnload >> execute [
	self workingCopies 
		do: [ :copy | self unload: copy ].
	self model load.
	self gofer metacelloCleanup.
	self workingCopies 
		do: [ :copy | self unregister: copy ]
]

{ #category : #unloading }
MetacelloTestsGoferUnload >> unloadClasses: aWorkingCopy [
	"skip this so that tests run faster"
]
