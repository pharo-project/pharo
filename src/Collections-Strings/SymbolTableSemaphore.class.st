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
	#classInstVars : [
		'enabled'
	],
	#category : 'Collections-Strings-Base',
	#package : 'Collections-Strings',
	#tag : 'Base'
}

{ #category : 'accessing' }
SymbolTableSemaphore class >> disable [
	enabled := false
]

{ #category : 'accessing' }
SymbolTableSemaphore class >> enable [
	enabled := true
]

{ #category : 'accessing' }
SymbolTableSemaphore class >> enabled [
	^ enabled ifNil: [ enabled := true ]
]

{ #category : 'instance creation' }
SymbolTableSemaphore class >> singleInstance [
	^ SingleInstance ifNil: [ SingleInstance := self forMutualExclusion ]
]
