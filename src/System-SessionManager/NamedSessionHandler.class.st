"
I'm a Class session handler that can be registered with a given name.
This name is a unique identifier and allows to easily replace currently registered session handler at this name with another session handler.
"
Class {
	#name : 'NamedSessionHandler',
	#superclass : 'ClassSessionHandler',
	#instVars : [
		'name'
	],
	#category : 'System-SessionManager-Handlers',
	#package : 'System-SessionManager',
	#tag : 'Handlers'
}

{ #category : 'instance creation' }
NamedSessionHandler class >> forClassNamed: aClassName id: aString [
	^ self new
		registeredClassName: aClassName;
		name: aString asSymbol;
		yourself
]

{ #category : 'accessing' }
NamedSessionHandler >> handlerId [

	^ name
]

{ #category : 'accessing' }
NamedSessionHandler >> name: aSymbol [ 
	name := aSymbol
]
