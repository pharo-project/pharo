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
MCPreambleDefinition >> load [
	super load.
	self evaluate
]
