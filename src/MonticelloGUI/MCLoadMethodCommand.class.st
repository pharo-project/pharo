Class {
	#name : 'MCLoadMethodCommand',
	#superclass : 'MCSnapshotBrowserCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCLoadMethodCommand class >> defaultName [

	^ 'Load method'
]

{ #category : 'executing' }
MCLoadMethodCommand >> execute [
	"Load the selected method"

	self selectedMethod ifNil: [ ^ self ].
	self selectedMethod isLoadable
		ifTrue: [ self selectedMethod load. ].
]

{ #category : 'accessing - selection' }
MCLoadMethodCommand >> selectedMethod [

	^ context selectedMethod
]
