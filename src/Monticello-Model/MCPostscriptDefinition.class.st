"
A postscript evaluated after load
"
Class {
	#name : 'MCPostscriptDefinition',
	#superclass : 'MCScriptDefinition',
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'accessing' }
MCPostscriptDefinition class >> scriptSelector [
	^ #postscript
]

{ #category : 'installing' }
MCPostscriptDefinition >> postload [
	self evaluate
]

{ #category : 'accessing' }
MCPostscriptDefinition >> sortKey [
	^ 'zzz' "force to the end so it gets loaded late"
]
