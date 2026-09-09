"
A preamble evaluated before load
"
Class {
	#name : 'MCPreambleDefinition',
	#superclass : 'MCScriptDefinition',
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'accessing' }
MCPreambleDefinition class >> scriptSelector [
	^ #preamble
]

{ #category : 'installing' }
MCPreambleDefinition >> loadWithLoader: aLoader [

	super loadWithLoader: aLoader.
	self evaluate
]
