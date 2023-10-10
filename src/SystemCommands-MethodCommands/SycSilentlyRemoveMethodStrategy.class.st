"
I am simple perform method remove operation without any extra logic
"
Class {
	#name : 'SycSilentlyRemoveMethodStrategy',
	#superclass : 'SycRemoveMethodStrategy',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'removing' }
SycSilentlyRemoveMethodStrategy >> removeMethods: methods [

	methods do: [ :method | method removeFromSystem ]
]

{ #category : 'removing' }
SycSilentlyRemoveMethodStrategy >> userRequestString [
	^'Remove it'
]
