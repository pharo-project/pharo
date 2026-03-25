"
The CommandLineArguments represents the arguments passed to the image.
In the following case,

	$PHARO_VM myImage.image --foo bar
	
`CommandLineArguments default` contains {'--foo'. 'bar'}.
"
Class {
	#name : 'CommandLineArguments',
	#superclass : 'Object',
	#instVars : [
		'arguments'
	],
	#category : 'System-CommandLineHandler-Base',
	#package : 'System-CommandLineHandler',
	#tag : 'Base'
}

{ #category : 'instance creation' }
CommandLineArguments class >> withArguments: aCollection [
	^ self basicNew
		initializeWithArguments: aCollection;
		yourself
]

{ #category : 'accessing' }
CommandLineArguments >> argumentAt: index [
	^ arguments at: index
]

{ #category : 'accessing' }
CommandLineArguments >> arguments [
	^ arguments asArray
]

{ #category : 'accessing' }
CommandLineArguments >> arguments: anArray [
	arguments := anArray
]

{ #category : 'initialization' }
CommandLineArguments >> commandLineArguments [
	"self commandLineArguments"

	| documentPath args |

	args := OrderedCollection withAll: Smalltalk arguments.
	documentPath := Smalltalk vm documentPath.
	documentPath isEmptyOrNil
		ifFalse: [ args addFirst: documentPath ].
 	^ args
]

{ #category : 'testing' }
CommandLineArguments >> hasArguments [
	^ arguments isNotEmpty 
]

{ #category : 'testing' }
CommandLineArguments >> hasOption: aString [
	| option |
	option := (aString size = 1
					ifTrue: [ '-']
					ifFalse: [ '--']), aString.
	(self arguments includes: option) ifTrue: [ ^ true ].
	option := option,'='.
	^ self arguments anySatisfy: [ :arg| arg beginsWith: option ]
]

{ #category : 'initialization' }
CommandLineArguments >> initialize [

	| documentPath  |

	arguments := OrderedCollection withAll: Smalltalk arguments.
	documentPath := Smalltalk vm documentPath.
	documentPath isEmptyOrNil
		ifFalse: [ arguments addFirst: documentPath ]
]

{ #category : 'initialization' }
CommandLineArguments >> initializeWithArguments: aCollection [
	super initialize.
	arguments := aCollection
]

{ #category : 'accessing' }
CommandLineArguments >> longOptionAt: aString ifAbsent: absentBlock [
	| optionStart |
	optionStart := '--' , aString , '='.
	^ self arguments
		detect: [ :arg | arg beginsWith: optionStart ]
		ifFound: [ :option | (option splitOn: '=') second ]
		ifNone: absentBlock
]

{ #category : 'accessing' }
CommandLineArguments >> optionAt: aString [
	^ self optionAt: aString ifAbsent: [
		Error signal: 'Could not find option ', aString ]
]

{ #category : 'accessing' }
CommandLineArguments >> optionAt: aString ifAbsent: absentBlock [
	^ (aString size = 1)
		ifTrue: [ self shortOptionAt: aString ifAbsent: absentBlock ]
		ifFalse: [ self longOptionAt: aString ifAbsent: absentBlock ]
]

{ #category : 'removing' }
CommandLineArguments >> removeFirst [

	arguments := arguments copyWithoutFirst
]

{ #category : 'accessing' }
CommandLineArguments >> shortOptionAt: aString ifAbsent: absentBlock [
	| index |
	index := (arguments indexOf: '-', aString) + 1.
	^ (index <= 1 or: [ index > arguments size ])
		ifFalse: [ arguments at: index ]
		ifTrue: absentBlock
]

{ #category : 'accessing' }
CommandLineArguments >> withFirstArgument: aBlock [
	self arguments ifEmpty: [ ^ self ].
	^ aBlock value: self arguments first
]
