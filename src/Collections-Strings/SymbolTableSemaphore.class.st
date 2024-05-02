"
I am a semaphore to protect the NewSymbols table from concurrent access when new symbols are internalized. 
I have a single instance 
"
Class {
	#name : 'SymbolTableSemaphore',
	#superclass : 'Semaphore',
	#classVars : [
		'SingleInstance'
	],
	#category : 'Collections-Strings-Base',
	#package : 'Collections-Strings',
	#tag : 'Base'
}

{ #category : 'instance creation' }
SymbolTableSemaphore class >> singleInstance [
	^ SingleInstance ifNil: [ SingleInstance := self forMutualExclusion ]
]
