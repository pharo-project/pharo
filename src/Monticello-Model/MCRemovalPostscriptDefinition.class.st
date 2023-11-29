"
A postscript evaluated after unloading/removing
"
Class {
	#name : 'MCRemovalPostscriptDefinition',
	#superclass : 'MCScriptDefinition',
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'accessing' }
MCRemovalPostscriptDefinition class >> scriptSelector [
	^ #postscriptOfRemoval 
]

{ #category : 'installing' }
MCRemovalPostscriptDefinition >> unload [
	super unload.
	self evaluate
]
