"
SUnit tests for class SHStyleElement
"
Class {
	#name : 'SHStyleElementTest',
	#superclass : 'TestCase',
	#category : 'Shout-Tests-Styling',
	#package : 'Shout-Tests',
	#tag : 'Styling'
}

{ #category : 'tests' }
SHStyleElementTest >> testSerialization [
	"STONing a StyleElement should result in a dictionary containing the attributes"
	| elem ston values  |
	elem := SHStyleElement group: 'default'.
	ston := String streamContents: [ :stream |
			        STON writer
				        on: stream;
				        nextPut: elem ].
	values := STON reader
		          on: ston readStream;
		          next.
	self assert: (values at: 'group') equals: elem group.
	self assert: (values at: 'color') equals: elem color.
	self assert: (values at: 'emphasis') equals: elem emphasis
]

{ #category : 'tests' }
SHStyleElementTest >> testSettingAttributes [

	| group element oldColor oldEmphasis newColor newEmphasis |
	group := 'default'.
	element := SHStyleElement group: group.
	self assert: element group equals: group.
	oldColor := element color.
	oldEmphasis := element emphasis.
	newColor := Color magenta.
	newEmphasis := 'bold'.
	element color: newColor.
	element emphasis: newEmphasis.
	self assert: element color equals: newColor.
	self assert: element emphasis equals: newEmphasis.
	element color: oldColor.
	element emphasis: oldEmphasis.
	self assert: element color equals: oldColor.
	self assert: element emphasis equals: oldEmphasis
]
