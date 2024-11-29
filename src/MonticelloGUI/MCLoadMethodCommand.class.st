Class {
	#name : 'MCLoadMethodCommand',
	#superclass : 'MCBrowserAbstractMethodCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCLoadMethodCommand class >> defaultName [

	^ 'Load method...'
]

{ #category : 'testing' }
MCLoadMethodCommand >> canBeExecuted [ 

	^ super canBeExecuted and: [ self selectedMethodDefinition isLoadable ]
]

{ #category : 'executing' }
MCLoadMethodCommand >> execute [
	"Load the selected method"

	self selectedMethod load
]
