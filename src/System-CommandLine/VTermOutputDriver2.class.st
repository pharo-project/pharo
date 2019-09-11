"
I convert high-level terminal commands  to characters sequences understood by the terminal.
I use a term info backend to communicate with the terminal. The default term info implementation is TermInfoCharacter.

I have two different style, the current styles (with any modifications the user made on it) and the installedStyle which is the style of the terminal.

I give access to the styles I'm not implementing with the method set
(For example self set:'1m'  will make the font bold)
I use a SharedPool named VTermOutputStyles
Example of usage:
| out |
	out := VTermOutputDriver2 stdout.
	out 
	tab;
	<< 'normal text with a tab';
	newLine;
	redFont;
	blueBackground: 'red text with blue background';
	newLine;
	bold;
	<< 'red bold text';
	bold;
	underline;
	newLine;
	<< 'underlined red bold text';
	insertLines:2;
	close

"
Class {
	#name : #VTermOutputDriver2,
	#superclass : #Object,
	#instVars : [
		'outStream',
		'terminfo',
		'style',
		'installedStyle'
	],
	#pools : [
		'VTermOutputStyles'
	],
	#category : #'System-CommandLine-VTerm-Driver'
}

{ #category : #'instance creation' }
VTermOutputDriver2 class >> on: anOutputStream [
	^ self new
		outStream: anOutputStream;
		yourself
]

{ #category : #'instance creation' }
VTermOutputDriver2 class >> stderr [
	^ self on: Stdio stderr
]

{ #category : #'instance creation' }
VTermOutputDriver2 class >> stdout [
	^ self on: Stdio stdout
]

{ #category : #'character printing' }
VTermOutputDriver2 >> << aStringOrCharacter [

	self nextPutAll: aStringOrCharacter asString
]

{ #category : #styles }
VTermOutputDriver2 >> addStyle: aString [
	"Add a character sequence to the current styles"

	self style addStyle: aString
]

{ #category : #accessing }
VTermOutputDriver2 >> background [

	^ self style background
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> backgroundColor: aColor [

	self background color: aColor
	
]

{ #category : #deprecated }
VTermOutputDriver2 >> backspace [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> black [
	^self blackFont.
]

{ #category : #deprecated }
VTermOutputDriver2 >> black: aString [
	^self blackFont:aString
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> blackBackground [
	
	self backgroundColor: Color black
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> blackBackground: aString [ 
	
	self write: aString withBackground: Color black
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> blackFont [

	self fontColor: Color black
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> blackFont: aString [ 
	
	self write: aString withColor: Color black
]

{ #category : #styles }
VTermOutputDriver2 >> blink [

	self addStyle: Blink
]

{ #category : #deprecated }
VTermOutputDriver2 >> blinking [
	^self blink
]

{ #category : #deprecated }
VTermOutputDriver2 >> blue [
	^self blueFont
]

{ #category : #deprecated }
VTermOutputDriver2 >> blue: aString [
	^self blueFont:aString
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> blueBackground [
	
	self backgroundColor: Color blue
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> blueBackground: aString [ 
	
	self write: aString withBackground: Color blue
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> blueFont [

	self fontColor: Color blue
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> blueFont: aString [ 
	
	self write: aString withColor: Color blue
]

{ #category : #styles }
VTermOutputDriver2 >> bold [

	self addStyle: Bold
]

{ #category : #styles }
VTermOutputDriver2 >> changeStyle [
	"Force the changes of style on the outStream"

	self style = installedStyle
		ifFalse: [ self style setInDriver: self ]
]

{ #category : #deprecated }
VTermOutputDriver2 >> clear [
	^self resetStyle
]

{ #category : #deprecated }
VTermOutputDriver2 >> clearFromBeginning [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> clearScreen [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> clearToEnd [
	^self
]

{ #category : #initialization }
VTermOutputDriver2 >> close [
	"Stdout shouldn't be closed"

	self resetStyle.
	outStream := nil.
]

{ #category : #testing }
VTermOutputDriver2 >> closed [

	^ outStream isNil or: [ outStream closed ]
]

{ #category : #deprecated }
VTermOutputDriver2 >> color: aColor [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> colorEscape [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> columns [
	^self
]

{ #category : #'character printing' }
VTermOutputDriver2 >> cr [
	"Print a cr (new line)"

	self nextPutAll:String cr
]

{ #category : #'character printing' }
VTermOutputDriver2 >> cr: anInteger [
	"Print anInteger cr (new line)"

	anInteger timesRepeat: [ self cr ]
]

{ #category : #deprecated }
VTermOutputDriver2 >> csiEscape [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> cursorEnhanced [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> cursorInvisible [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> cursorInvisibleright [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> cursorNormal [
^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> cyan [
	^self cyanFont
]

{ #category : #deprecated }
VTermOutputDriver2 >> cyan: aString [
	^self cyanFont: aString
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> cyanBackground [
	
	self backgroundColor: Color cyan
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> cyanBackground: aString [ 
	
	self write: aString withBackground: Color cyan
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> cyanFont [

	self fontColor: Color cyan
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> cyanFont: aString [ 
	
	self write: aString withColor: Color cyan
]

{ #category : #deprecated }
VTermOutputDriver2 >> deleteCharacter [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> deleteLine [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> down [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> down: aString [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> endInsertMode [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> erase [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> errorColor [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> escape [
^self
]

{ #category : #accessing }
VTermOutputDriver2 >> flush [
	"Flush the outStream to make the command sent be executed now"

	self outStream flush
]

{ #category : #accessing }
VTermOutputDriver2 >> font [

	^ self style font
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> fontColor: aColor [

	self font color: aColor.
	
	
]

{ #category : #deprecated }
VTermOutputDriver2 >> green [
	^self greenFont
]

{ #category : #deprecated }
VTermOutputDriver2 >> green: aString [
	^self greenFont: aString
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> greenBackground [
	
	self backgroundColor: Color green
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> greenBackground: aString [ 
	
	self write: aString withBackground: Color green
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> greenFont [

	self fontColor: Color green
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> greenFont: aString [ 
	
	self write: aString withColor: Color green
]

{ #category : #deprecated }
VTermOutputDriver2 >> here [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> home [
	^self
]

{ #category : #initialization }
VTermOutputDriver2 >> initialize [

	super initialize.
	style := VTermOutputStyle new.
	installedStyle := style copy.
]

{ #category : #deprecated }
VTermOutputDriver2 >> insertCharacter [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> insertLine [
	^self newLine
]

{ #category : #'character printing' }
VTermOutputDriver2 >> insertLines: anInteger [

	anInteger timesRepeat: [ self newLine ]
	
]

{ #category : #deprecated }
VTermOutputDriver2 >> insertMode [
	^self
]

{ #category : #accessing }
VTermOutputDriver2 >> installedStyle [

	^installedStyle
]

{ #category : #accessing }
VTermOutputDriver2 >> installedStyle: aStyle [

	installedStyle := aStyle
]

{ #category : #deprecated }
VTermOutputDriver2 >> invisible [
	^self
]

{ #category : #styles }
VTermOutputDriver2 >> invisibleFont [
	"Set the current style to invisible font"

	self addStyle: InvisibleFont
]

{ #category : #deprecated }
VTermOutputDriver2 >> left [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> left: aString [
	^self
]

{ #category : #'character printing' }
VTermOutputDriver2 >> lf [
	"Print a lf (new line)"

	self nextPutAll:String lf
]

{ #category : #'character printing' }
VTermOutputDriver2 >> lf: anInteger [
	"Print anInteger lf (new line)"

	anInteger timesRepeat: [ self lf ]
]

{ #category : #deprecated }
VTermOutputDriver2 >> light [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> lines [
	^self
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> magentaBackground [
	
	self backgroundColor: Color magenta
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> magentaBackground: aString [ 
	
	self write: aString withBackground: Color magenta
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> magentaFont [

	self fontColor: Color magenta
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> magentaFont: aString [ 
	
	self write: aString withColor: Color magenta
]

{ #category : #'character printing' }
VTermOutputDriver2 >> newLine [

	self nextPutAll: OSPlatform current lineEnding
]

{ #category : #'character printing' }
VTermOutputDriver2 >> nextPut: aCharacter [
	"When a character is put, force the style changes"

	self changeStyle.
	self outStream nextPut: aCharacter.
	self flush
]

{ #category : #'character printing' }
VTermOutputDriver2 >> nextPutAll: aString [

	aString do: [ :v | self nextPut: v ]
]

{ #category : #deprecated }
VTermOutputDriver2 >> normal [
	^self
]

{ #category : #accessing }
VTermOutputDriver2 >> outStream [

	^outStream
]

{ #category : #accessing }
VTermOutputDriver2 >> outStream: stream [

	outStream := stream.
]

{ #category : #deprecated }
VTermOutputDriver2 >> overwrite: aString [
	^self
]

{ #category : #behavior }
VTermOutputDriver2 >> peekLast [
	"Copy MultiByteFileStream behavior
	This method is needed by SmalltalkImage>>openLog (for example)"

	^ nil
]

{ #category : #deprecated }
VTermOutputDriver2 >> pink [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> pink: aString [
	^self
]

{ #category : #behavior }
VTermOutputDriver2 >> position [
	"Copy MultiByteFileStream behavior
	This method is needed by SmalltalkImage>>openLog (for example)"

	^ 0
]

{ #category : #'character printing' }
VTermOutputDriver2 >> print: anObject [
	"Have anObject print it self"

	self nextPutAll: anObject asString
]

{ #category : #deprecated }
VTermOutputDriver2 >> red [
	^self redFont
]

{ #category : #deprecated }
VTermOutputDriver2 >> red: aString [
	^self redFont: aString
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> redBackground [
	
	self backgroundColor: Color red
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> redBackground: aString [ 
	
	self write: aString withBackground: Color red
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> redFont [

	self fontColor: Color red
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> redFont: aString [ 
	
	self write: aString withColor: Color red
]

{ #category : #initialization }
VTermOutputDriver2 >> reset [
	^self resetStyle
]

{ #category : #reset }
VTermOutputDriver2 >> resetStyle [
	"Reset the style by creating a new VTermOutputStyle and reseting the terminal to default config"

	style := VTermOutputStyle new.
	installedStyle := style copy.
	self set: ResetStyle
]

{ #category : #deprecated }
VTermOutputDriver2 >> restoreCursor [
	^self
]

{ #category : #styles }
VTermOutputDriver2 >> reverse [
	"Set the current style to reversed"

	self addStyle: Reverse
]

{ #category : #deprecated }
VTermOutputDriver2 >> right [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> right: aString [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> saveCursor [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> scrollBackward [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> scrollForward [
	^self
]

{ #category : #styles }
VTermOutputDriver2 >> set: aString [
	"Make possible the manual changes self set:'1m' will make the font bold for example"

	self terminfo set: aString
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> setBackgroundColor: aColor [

	self terminfo setBackgroundColor: aColor
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> setFontColor: aColor [ 
	
	self terminfo setFontColor: aColor
]

{ #category : #'character printing' }
VTermOutputDriver2 >> space [

	self nextPut: Character space.
]

{ #category : #'character printing' }
VTermOutputDriver2 >> space: anInteger [

	anInteger timesRepeat: [ self space ].
]

{ #category : #deprecated }
VTermOutputDriver2 >> ss3Escape [
	^self
]

{ #category : #styles }
VTermOutputDriver2 >> standout [
	"Set the current style to standout"

	self addStyle: Standout
]

{ #category : #deprecated }
VTermOutputDriver2 >> startOfLine [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> store: aString [
	^self
]

{ #category : #accessing }
VTermOutputDriver2 >> style [
	^ style
]

{ #category : #accessing }
VTermOutputDriver2 >> style: aStyle [
	style := aStyle.
	
	
]

{ #category : #accessing }
VTermOutputDriver2 >> styles [
	^self style styles
]

{ #category : #behavior }
VTermOutputDriver2 >> tab [
	self nextPut: Character tab.
]

{ #category : #deprecated }
VTermOutputDriver2 >> termcap [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> termcap: aString [
	^self
]

{ #category : #accessing }
VTermOutputDriver2 >> terminfo [
	^ terminfo
		ifNil: [ self terminfo: (OSPlatform current newTerminfo on: self outStream) ]
]

{ #category : #accessing }
VTermOutputDriver2 >> terminfo: aTerminfo [
	terminfo := aTerminfo.
]

{ #category : #deprecated }
VTermOutputDriver2 >> topLeft [
	^self
]

{ #category : #styles }
VTermOutputDriver2 >> underline [
	"Set the current style to underlined"

	self addStyle: Underline
]

{ #category : #deprecated }
VTermOutputDriver2 >> underlineOff [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> up [
	^self
]

{ #category : #deprecated }
VTermOutputDriver2 >> up: aString [
	^self
]

{ #category : #coloring }
VTermOutputDriver2 >> useStyle: newStyle during: aBlockClosure [
	"Execute aBlockClosure with newStyle and set back the style to the old one after"

	| oldStyle |
	oldStyle := self style.
	self style: newStyle.
	aBlockClosure
		ensure: [ self style: oldStyle ]
]

{ #category : #deprecated }
VTermOutputDriver2 >> white [
	^self whiteFont
]

{ #category : #deprecated }
VTermOutputDriver2 >> white: aString [
	^self whiteFont:aString
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> whiteBackground [
	
	self backgroundColor: Color white
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> whiteBackground: aString [ 
	
	self write: aString withBackground: Color white
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> whiteFont [
	self fontColor: Color white
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> whiteFont: aString [ 
	
	self write: aString withColor: Color white
]

{ #category : #deprecated }
VTermOutputDriver2 >> width [
	^self
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> write: aString withBackground: aColor [
	"Print aString with the current style and with aColor background without modifying the current style"

	| tempStyle |
	tempStyle := self style copy.
	tempStyle background color: aColor.
	self
		useStyle: tempStyle
		during: [ self nextPutAll: aString ]
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> write: aString withColor: aColor [
	"Print aString with the current style and with aColor font without modifying the current style"

	| newStyle |
	newStyle := self style copy.
	newStyle font color: aColor.
	self
		useStyle: newStyle
		during: [ self nextPutAll: aString ]
]

{ #category : #deprecated }
VTermOutputDriver2 >> yellow [
	^self yellowFont
]

{ #category : #deprecated }
VTermOutputDriver2 >> yellow: aString [
	^self yellowFont:aString
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> yellowBackground [
	
	self backgroundColor: Color yellow
]

{ #category : #'background coloring' }
VTermOutputDriver2 >> yellowBackground: aString [ 
	
	self write: aString withBackground: Color yellow
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> yellowFont [
	self fontColor: Color yellow
]

{ #category : #'text coloring' }
VTermOutputDriver2 >> yellowFont: aString [ 
	
	self write: aString withColor: Color yellow
]
