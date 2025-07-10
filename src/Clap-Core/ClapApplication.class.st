"
I define basic boilerplate for delegating commandline behavior to the instance side, passing the arguments match.

Declare the main commandline in a class-side method, concluding the main meaning block as follows:

somethingCommandline
	<commandline>
	^ (ClapCommand withName: 'something')
		""... description, parameters, etc""
		meaning: [ :args |
			""... preliminary checks""
			(self with: args) execute ]
"
Class {
	#name : 'ClapApplication',
	#superclass : 'Object',
	#instVars : [
		'context'
	],
	#category : 'Clap-Core-Specification',
	#package : 'Clap-Core',
	#tag : 'Specification'
}

{ #category : 'instance creation' }
ClapApplication class >> with: aClapContext [
	^ self new
		setContext: aClapContext;
		yourself
]

{ #category : 'accessing' }
ClapApplication >> commandSpecIdentifiers [
	^ self commandSpecs collect: [ :spec | spec identifier ]
]

{ #category : 'accessing' }
ClapApplication >> commandSpecs [

	^ self context commandMatches collect: [ :match |
		  match specification ]
]

{ #category : 'accessing' }
ClapApplication >> context [
	^ context
]

{ #category : 'private' }
ClapApplication >> defaultValueFor: aFlagIdentifier [

	^ self context
		  defaultValueFor: aFlagIdentifier asSymbol
		  in: self
		  noneBlock: [ NotFound signal: 'Cannot find ' , aFlagIdentifier asSymbol, '!' ]
]

{ #category : 'execution' }
ClapApplication >> execute [
	self subclassResponsibility
]

{ #category : 'execution' }
ClapApplication >> executeOrPrintHelp [
	(self hasFlag: #help)
		ifTrue: [ self printHelp. self exitSuccess ]
		ifFalse: [ self execute ]
]

{ #category : 'execution' }
ClapApplication >> exitSuccess [
	^ self context exitSuccess
]

{ #category : 'accessing' }
ClapApplication >> flags [ 
	^ self context flags
		collect: [ :match | match specification identifier ]
]

{ #category : 'testing' }
ClapApplication >> hasFlag: anIdentifier [ 
	
	^ self context hasFlag: anIdentifier asSymbol
]

{ #category : 'execution' }
ClapApplication >> outputStreamDo: aBlock [
	aBlock value: self context stdout
]

{ #category : 'accessing' }
ClapApplication >> positional: aFlagIdentifier [ 
	"Get the value of a positional with given identifier"
	
	^ [ self rawPositional: aFlagIdentifier ]
		on: NotFound 
		do: [ self defaultValueFor: aFlagIdentifier ]
]

{ #category : 'accessing' }
ClapApplication >> positional: anIdentifier ifPresent: aBlock [
	"Execute aBlock if a positional with the identifier is found. The positional valu will be passed as first argument of the block"
	
	[ aBlock value: (self positional: anIdentifier) ]
		on: NotFound 
		do: [ "ignore" ]
]

{ #category : 'execution' }
ClapApplication >> printHelp [ 
	(ClapDocumenter on: (ZnCharacterWriteStream on: self context stdout))
			explainContext: self context
			
]

{ #category : 'private' }
ClapApplication >> rawPositional: anIdentifier [
	"We get positionals but a NotFound error can be signaled."
	
	| matches values |
	matches := self context positional: anIdentifier asSymbol.
	values := matches collect: [ :match |
		          match specification isFlag
			          ifTrue: [ match child value ]
			          ifFalse: [ match value ] ].
		
	^ matches first specification isMultiple
		ifTrue: [ values ]
		ifFalse: [ values first ]
]

{ #category : 'accessing' }
ClapApplication >> setContext: aClapContext [
	context := aClapContext
]
