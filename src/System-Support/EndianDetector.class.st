"
I am EndianDetector, a class responsible of detecting the endianess in the current machine.

I do so in my #calcEndianess method, which works as follows:

 - it creates a float object, whose endianess is managed automatically by the VM.
 - copy the raw data of that float object into a bytes object (instance of my class)
 - then, we can check in the raw copy if the data of the float is in the least or more significant word of the object.

Internally, I do the raw data copy by changing the class of the float into me. To do that I have the following properties:
- I'm a compact class (required to change the class)
- I'm in the startup list to check this on every startup
"
Class {
	#name : #EndianDetector,
	#superclass : #Object,
	#type : #words,
	#classVars : [
		'EndianCache'
	],
	#category : #System-Support
}

{ #category : #endian }
EndianDetector class >> calcEndianness [
	"store the least significant word of the float with 1; on big endian machines this is the second word"
	| f |
	f := Float new: 2.
	f at: 2 put: 1.
	self adoptInstance: f.
	^ (f at: 1) = 1 ifTrue: [#little] ifFalse: [#big]
]

{ #category : #endian }
EndianDetector class >> endianness [

	EndianCache ifNil: [EndianCache := self calcEndianness].
	^ EndianCache.

]

{ #category : #'class initialization' }
EndianDetector class >> initialize [

	SessionManager default
		registerSystemClassNamed: self name
]

{ #category : #testing }
EndianDetector class >> isBigEndian [
	
	^ self isLittleEndian not
]

{ #category : #testing }
EndianDetector class >> isLittleEndian [
	
	^ self endianness == #little
]

{ #category : #'system startup' }
EndianDetector class >> startUp: isImageStarting [

	"only handle when lauching a new image"
	isImageStarting ifFalse: [ ^ self ].
	EndianCache := nil.
]
