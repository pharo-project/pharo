Class {
	#name : 'MCBrowserAbstractMethodCommand',
	#superclass : 'MCSnapshotBrowserCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'testing' }
MCBrowserAbstractMethodCommand >> canBeExecuted [ 

	^ self selectedMethod notNil
]

{ #category : 'accessing - selection' }
MCBrowserAbstractMethodCommand >> selectedMethod [
	"Answer the actual selected <CompiledMethod>"

	^ self selectedMethodDefinition method
]

{ #category : 'accessing - selection' }
MCBrowserAbstractMethodCommand >> selectedMethodDefinition [
	"Answer the currently selected <MCMethodDefinition>"

	^ context selectedMethod
]
