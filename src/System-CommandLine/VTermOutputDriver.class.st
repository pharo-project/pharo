"
I convert high-level terminal commands like moving the cursor or setting the color to characters sequences understood by the terminal.

Furthermore I do some bookkeeping to reduce the number of characters sent to the terminal.

Example of usage:

	out := VTermOutputDriver stdout.
	out << 'normal text'.
	out lf.
	'red text' do: [ :c | out color256: Color red. out << c ].
	out lf.
	'bold text' do: [ :c | out bold. out << c ].
	out clear.
	out lf.
"
Class {
	#name : #VTermOutputDriver,
	#superclass : #Object,
	#instVars : [
		'termcap',
		'outStream',
		'currentBackground',
		'currentColor',
		'light',
		'blink'
	],
	#category : #'System-CommandLine-VTerm-Driver'
}

{ #category : #'instance creation' }
VTermOutputDriver class >> on: anOutputStream [
	^ self new
		outStream: (ZnNewLineWriterStream on:
			(ZnCharacterWriteStream on: anOutputStream encoding: 'utf8'));
		yourself
]

{ #category : #'instance creation' }
VTermOutputDriver class >> stderr [
	^ self on: Stdio stderr
]

{ #category : #'instance creation' }
VTermOutputDriver class >> stdout [
	^ self on: Stdio stdout
]

{ #category : #writing }
VTermOutputDriver >> << aStringOrCharacter [
	aStringOrCharacter putOn: outStream.
]

{ #category : #removing }
VTermOutputDriver >> backspace [
	outStream nextPut: Character backspace.
	self deleteCharacter 
]

{ #category : #coloring }
VTermOutputDriver >> black [
	self color: 30.
]

{ #category : #coloring }
VTermOutputDriver >> black: aString [
	self black; << aString; flush; clear.
]

{ #category : #highlighting }
VTermOutputDriver >> blinking [
	self termcap: 'mb'
]

{ #category : #coloring }
VTermOutputDriver >> blue [
	self color: 34.
]

{ #category : #coloring }
VTermOutputDriver >> blue: aString [
	self blue; << aString; flush; clear.
]

{ #category : #highlighting }
VTermOutputDriver >> bold [
	" double bright mode "
	self termcap: 'md'
]

{ #category : #accessing }
VTermOutputDriver >> clear [
	self reset.
	self colorEscape.
]

{ #category : #removing }
VTermOutputDriver >> clearFromBeginning [
	self termcap: 'cb'
]

{ #category : #removing }
VTermOutputDriver >> clearScreen [
	self termcap: 'cl'
]

{ #category : #removing }
VTermOutputDriver >> clearToEnd [
	self termcap: 'ce'
]

{ #category : #testing }
VTermOutputDriver >> closed [
	^ outStream isNil or: [ outStream closed ]
]

{ #category : #coloring }
VTermOutputDriver >> color: aColor [
	currentColor := aColor.
	self colorEscape.
]

{ #category : #escaping }
VTermOutputDriver >> colorEscape [
	self csiEscape.
	blink ifTrue: [ outStream nextPutAll: '5;' ].
	outStream print: currentColor.
	(currentBackground == 0) ifFalse: [
		outStream nextPut: $;; print: currentBackground ].
	outStream nextPut: $m.
]

{ #category : #accessing }
VTermOutputDriver >> columns [
	^ termcap getnum: 'co'
]

{ #category : #navigating }
VTermOutputDriver >> cr [
	self termcap: 'cr'
]

{ #category : #escaping }
VTermOutputDriver >> csiEscape [
	"Control Sequence Introducer escape"
	outStream nextPut: Character escape; nextPut: $[
]

{ #category : #cursor }
VTermOutputDriver >> cursorEnhanced [
	self termcap: 'vs'
]

{ #category : #cursor }
VTermOutputDriver >> cursorInvisible [
	self termcap: 'vi'
]

{ #category : #cursor }
VTermOutputDriver >> cursorNormal [
	self termcap: 've'
]

{ #category : #coloring }
VTermOutputDriver >> cyan [
	self color: 36.
]

{ #category : #coloring }
VTermOutputDriver >> cyan: aString [
	self cyan; << aString; flush; clear.
]

{ #category : #removing }
VTermOutputDriver >> deleteCharacter [
	self termcap: 'dc'
]

{ #category : #removing }
VTermOutputDriver >> deleteLine [
	self termcap: 'dl'
]

{ #category : #navigating }
VTermOutputDriver >> down [
	self termcap: 'do'
]

{ #category : #navigating }
VTermOutputDriver >> down: distance [
	distance < 0
		ifTrue: [ 0 - distance timesRepeat: [ self up ]]
		ifFalse: [ distance timesRepeat: [ self down ]].
]

{ #category : #mode }
VTermOutputDriver >> endInsertMode [
	self termcap: 'ei'
]

{ #category : #removing }
VTermOutputDriver >> erase [
	"outStream nextPutAll:
		(termcap parm: '1' in:
			(termcap getstr: 'ec'))"
	self shouldBeImplemented.
]

{ #category : #coloring }
VTermOutputDriver >> errorColor [ 
	self red
]

{ #category : #escaping }
VTermOutputDriver >> escape [
	outStream nextPut: Character escape
]

{ #category : #writing }
VTermOutputDriver >> flush [
	outStream flush.
]

{ #category : #coloring }
VTermOutputDriver >> green [
	self color: 32.
]

{ #category : #coloring }
VTermOutputDriver >> green: aString [
	self green; << aString; flush; clear.
]

{ #category : #navigating }
VTermOutputDriver >> here [
	self flag: #todo. "use termcap here"
	self csiEscape.
	outStream nextPut: $E
]

{ #category : #navigating }
VTermOutputDriver >> home [
	self termcap: 'ho'
]

{ #category : #initialization }
VTermOutputDriver >> initialize [ 
	super initialize.
	termcap := Termcap new.
	self reset.
]

{ #category : #inserting }
VTermOutputDriver >> insertCharacter [
	self termcap:  'ic'
]

{ #category : #inserting }
VTermOutputDriver >> insertLine [
	self termcap: 'al'.
	self lf
]

{ #category : #mode }
VTermOutputDriver >> insertMode [
	self termcap: 'im'
]

{ #category : #highlighting }
VTermOutputDriver >> invisible [
	self termcap: 'mk'
]

{ #category : #navigating }
VTermOutputDriver >> left [
	self termcap:  'le'
]

{ #category : #navigating }
VTermOutputDriver >> left: distance [
	distance < 0
		ifTrue: [ 0 - distance timesRepeat: [ self right ]]
		ifFalse: [ distance timesRepeat: [ self left ]].
]

{ #category : #writing }
VTermOutputDriver >> lf [
	outStream lf
]

{ #category : #coloring }
VTermOutputDriver >> light [
	light := true.
]

{ #category : #accessing }
VTermOutputDriver >> lines [
	^ termcap getnum: 'li'
]

{ #category : #printing }
VTermOutputDriver >> nextPut: aCharacter [ 
	outStream nextPut: aCharacter.
]

{ #category : #printing }
VTermOutputDriver >> nextPutAll: aString [
	outStream nextPutAll: aString
]

{ #category : #highlighting }
VTermOutputDriver >> normal [
	self termcap: 'me'
]

{ #category : #accessing }
VTermOutputDriver >> outStream [
	^ outStream
]

{ #category : #accessing }
VTermOutputDriver >> outStream: stream [
	outStream := stream.
]

{ #category : #writing }
VTermOutputDriver >> overwrite: char [
	self flag: #TODO. "properly implement this"
	outStream nextPut: char
]

{ #category : #coloring }
VTermOutputDriver >> pink [
	self color: 35.
]

{ #category : #coloring }
VTermOutputDriver >> pink: aString [
	self pink; nextPutAll: aString; flush; clear.
]

{ #category : #printing }
VTermOutputDriver >> print: anObject [ 
	outStream print: anObject
]

{ #category : #coloring }
VTermOutputDriver >> red [
	self color: 31.
]

{ #category : #coloring }
VTermOutputDriver >> red: aString [
	self red; << aString; flush; clear.
]

{ #category : #initialization }
VTermOutputDriver >> reset [
	currentColor := 0.
	currentBackground := 0.
	light := false.
	blink := false.
]

{ #category : #navigating }
VTermOutputDriver >> restoreCursor [
	self termcap: 'rc'
]

{ #category : #highlighting }
VTermOutputDriver >> reverse [
	self termcap: 'mr'
]

{ #category : #navigating }
VTermOutputDriver >> right [
	self termcap: 'nd'
]

{ #category : #navigating }
VTermOutputDriver >> right: distance [
	distance < 0
		ifTrue: [ 0 - distance timesRepeat: [ self left ]]
		ifFalse: [ distance timesRepeat: [ self right ]].
]

{ #category : #navigating }
VTermOutputDriver >> saveCursor [
	outStream nextPutAll: (termcap getstr: 'sc')
]

{ #category : #scrolling }
VTermOutputDriver >> scrollBackward [
	self termcap: 'sr'
]

{ #category : #scrolling }
VTermOutputDriver >> scrollForward [
	self termcap: 'sf'
]

{ #category : #writing }
VTermOutputDriver >> space [
	outStream space
]

{ #category : #escaping }
VTermOutputDriver >> ss3Escape [
	"Control Sequence Introducer escape"
	outStream nextPut: Character escape; nextPut: $O
]

{ #category : #navigating }
VTermOutputDriver >> startOfLine [
	^ self cr
]

{ #category : #printing }
VTermOutputDriver >> store: anObject [ 
	outStream store: anObject
]

{ #category : #writing }
VTermOutputDriver >> tab [
	outStream tab
]

{ #category : #accessing }
VTermOutputDriver >> termcap [
	^ termcap
]

{ #category : #private }
VTermOutputDriver >> termcap: termcapCapabilityIdentifier [
	"Output the termcap command sequence for the identifier.
	Use #basicNextPutAll: to avoid possible conversions applied by the stream"
	outStream basicNextPutAll: (termcap getstr: termcapCapabilityIdentifier)
]

{ #category : #navigating }
VTermOutputDriver >> topLeft [
	self termcap: 'ho'
]

{ #category : #highlighting }
VTermOutputDriver >> underline [
	self termcap: 'us'
]

{ #category : #highlighting }
VTermOutputDriver >> underlineOff [
	self termcap: 'ue'
]

{ #category : #navigating }
VTermOutputDriver >> up [
	self termcap: 'up'
]

{ #category : #navigating }
VTermOutputDriver >> up: distance [
	distance < 0
		ifTrue: [ 0 - distance timesRepeat: [ self down ]]
		ifFalse: [ distance timesRepeat: [ self up ]].
]

{ #category : #coloring }
VTermOutputDriver >> white [
	self color: 37.
]

{ #category : #coloring }
VTermOutputDriver >> white: aString [
	self white; << aString; flush; clear.
]

{ #category : #accessing }
VTermOutputDriver >> width [
	^ self columns
]

{ #category : #coloring }
VTermOutputDriver >> yellow [
	self color: 33.
]

{ #category : #coloring }
VTermOutputDriver >> yellow: aString [
	self yellow; << aString; flush; clear.
]
