"
Transcripter is a dog-simple scrolling stream with display.  It is intended to operate with no support from MVC or color in a minimal, or headless version of Pharo.  No attention has been paid to appearance or performance.

type exit (without quote) to exit the evaluator
type revert (without quote) to revert the last changes.

Transcripter askForEmergencyEvaluatorOrExitWithText: 'Emergency Evaluator'

Transcripter emergencyEvaluator
"
Class {
	#name : #Transcripter,
	#superclass : #ReadWriteStream,
	#instVars : [
		'frame',
		'para'
	],
	#classVars : [
		'Icon'
	],
	#pools : [
		'EventSensorConstants'
	],
	#category : #EmergencyEvaluator
}

{ #category : #launching }
Transcripter class >> askForEmergencyEvaluatorOrExitWithText: aText [ 
	| m |
	m := String streamContents: [:s | 
					s nextPutAll: aText.
					s cr; nextPutAll: '-----------------------------------------------------------------'.
					s cr; nextPutAll: 'Type CR to enter an emergency evaluator.'.
					s cr; nextPutAll: 'Type any other character to restart.'].
	Display getCanvas 
		drawString: m
		from: 1
		to: m size
		autoBoundAt: 0 @ 0
		font: nil
		color: Color black.
	self nextChar = Character cr
		ifTrue: [self emergencyEvaluator]
]

{ #category : #utilities }
Transcripter class >> buildIcon [

	| bitmap |
	bitmap := (Bitmap new: 256) copyFromByteArray:   #[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 170 128 0 0 0 0 0 52 255 186 0 9 142 113 0 10 153 102 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 182 146 0 27 179 132 0 20 255 153 0 31 156 107 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 29 202 149 0 51 205 145 0 48 234 112 0 233 254 111 0 95 244 121 0 33 209 155 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 46 0 0 0 189 46 20 0 245 102 44 0 255 159 69 0 198 215 93 0 22 197 116 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 0 0 0 49 0 0 0 96 0 0 0 81 0 0 0 225 0 0 0 191 0 0 0 86 21 9 0 42 79 36 0 55 9 5 0 49 167 120 0 3 170 85 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 93 0 0 0 236 0 3 0 255 12 34 12 255 36 66 36 255 26 42 26 255 0 0 0 188 0 0 0 33 0 0 0 0 0 0 0 48 0 0 0 2 128 128 0 1 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 150 0 0 0 255 0 16 0 255 27 89 27 255 77 136 77 255 116 162 116 255 107 135 107 255 67 83 67 255 73 97 73 245 2 3 2 46 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 83 0 0 0 255 0 6 0 255 0 77 0 255 51 119 51 255 102 153 102 255 151 185 151 255 191 212 191 255 200 218 200 255 168 197 168 255 65 89 65 209 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 201 0 0 0 255 0 47 0 255 5 88 5 255 58 124 58 255 111 159 111 255 165 195 165 255 218 230 218 255 238 244 238 255 185 208 185 255 130 171 130 255 10 18 10 77 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 247 0 0 0 255 0 68 0 255 0 85 0 255 51 119 51 255 102 153 102 255 150 185 150 255 189 211 189 255 198 217 198 255 166 196 166 255 120 165 120 255 25 47 25 135 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 15 0 0 0 255 0 0 0 255 0 74 0 255 0 85 0 255 31 105 31 255 76 135 76 255 115 161 115 255 141 179 141 255 146 182 146 255 127 169 127 255 91 146 91 255 21 50 21 155 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 220 0 0 0 255 0 57 0 255 0 85 0 255 1 86 1 255 39 111 39 255 71 132 71 255 90 145 90 255 93 147 93 255 80 138 80 255 52 120 52 255 4 23 4 99 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 143 0 0 0 255 0 23 0 255 0 85 0 255 0 85 0 255 0 85 0 255 23 100 23 255 38 110 38 255 40 112 40 255 30 105 30 255 6 73 6 244 0 2 0 28 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9 0 0 0 213 0 0 0 255 0 45 0 255 0 85 0 255 0 85 0 255 0 85 0 255 0 85 0 255 0 85 0 255 0 77 0 255 0 15 0 105 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 128 128 128 68 68 68 68 206 11 11 11 255 0 23 0 255 0 58 0 255 0 74 0 255 0 67 0 255 0 47 0 249 1 7 1 155 31 31 31 57 130 130 130 22 128 128 128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 17 135 135 135 63 125 125 125 88 128 128 128 147 57 57 57 194 28 28 28 227 12 12 12 214 19 19 19 181 38 38 38 120 91 91 91 92 128 128 128 80 128 128 128 41 124 124 124 2 128 128 128 0 0 0 0].
	^ Form extent: 16@16 depth: 32 bits: bitmap
]

{ #category : #launching }
Transcripter class >> emergencyEvaluator [
	"Display and launch an evaluator"
	
	(self newInFrame: (0@0 corner: 420@300))
		show: 'Type revert (without quote) to revert your last method change.
Type exit (without quote) to exit the emergency evaluator.';
		readEvalPrint.
]

{ #category : #menu }
Transcripter class >> emergencyEvaluatorMenuOn: aBuilder [ 
	<worldMenu> 
	
	(aBuilder item: #'Emergency Evaluator')
		action: [self emergencyEvaluator. World restoreMorphicDisplay];
		parent: #Tools;
		order: 0.57;	
		help: 'Invoke the emergency evaluator.';
		icon: self icon
	
]

{ #category : #utilities }
Transcripter class >> icon [

	^ Icon ifNil: [Icon := self buildIcon]
]

{ #category : #utilities }
Transcripter class >> newInFrame: frame [ 
	"Create a transcript of a given size. Does not launch the evaluator"
	
	"
	(Transcripter newInFrame: (0@0 extent: 100@200))
	nextPutAll: 'Hello there'; endEntry;
	cr; print: 355.0/113; endEntry;
	readEvalPrint
	"
	| transcript |
	transcript := self on: (String new: 100).
	transcript initInFrame: frame.
	^ transcript clear
]

{ #category : #utilities }
Transcripter class >> nextChar [
	[| evt type | 
		(Delay forMilliseconds: 20) wait.
		Sensor nextEvent
		ifNotNil: [:evtBuf | 
			type := evtBuf first.
			type = EventTypeKeyboard
				ifTrue: [evt := World currentHand generateKeyboardEvent: evtBuf.
					evt isKeystroke
						ifTrue: [| c |
							c := evt keyCharacter.
							(c asciiValue < 128
									or: [c isSeparator
											or: [c isSpecial
													or: [c = Character backspace]]])
								ifTrue: [Sensor flushKeyboard.
									^ evt keyCharacter]]]]] repeat
]

{ #category : #utilities }
Transcripter class >> startTranscriptProcess [
	"Transcripter startTranscriptProcess. Pay attention exit will quit the image."
	
	| activeProcess |
	Transcript := self newInFrame: Display boundingBox.
	activeProcess := [ Transcript readEvalPrint.
					Smalltalk session runShutdown: true.
				 	Smalltalk quitPrimitive ] newProcess priority: Processor userSchedulingPriority.
	activeProcess resume.
	Processor terminateActive 
]

{ #category : #private }
Transcripter >> black [
	Display depth = 1 ifTrue: [^ Bitmap with: 16rFFFFFFFF "Works without color support"].
	^ Color black
]

{ #category : #accessing }
Transcripter >> clear [
	self clearFrame.
	self on: (String new: 100);  endEntry 
]

{ #category : #private }
Transcripter >> clearFrame [
	Display getCanvas
		frameAndFillRectangle: frame
		fillColor: self white
		borderWidth: 2
		borderColor: self black
]

{ #category : #accessing }
Transcripter >> compose [ 
	para
		compose: self contents asText 
		style: self textStyle
		from: 1
		in: ((frame insetBy: 4) withHeight: 9999).

]

{ #category : #'command line' }
Transcripter >> confirm: queryString [ 
	| choice |
	[true]
		whileTrue: 
			[choice := self request: queryString , '
Please type Yes or No followed by return
> '.
			choice first asUppercase = $Y ifTrue: [^ true].
			choice first asUppercase = $N ifTrue: [^ false]]
]

{ #category : #accessing }
Transcripter >> display [
	self clearFrame.
	Display getCanvas
		paragraph: para
		bounds: frame
		color: self black
]

{ #category : #accessing }
Transcripter >> endEntry [
	| c d cb |
	c := self contents.
	Display extent ~= DisplayScreen actualScreenSize
		ifTrue: ["Handle case of user resizing physical window"
			DisplayScreen startUp.
			frame := frame intersect: Display boundingBox.
			^ self clear; show: c].
	self compose.
	d := para extent y - frame bottom.
	d > 0
		ifTrue: ["Scroll up to keep all contents visible"
			cb := para characterBlockAtPoint: para compositionRectangle topLeft + (0 @ (d + self textStyle lineGrid)).
			self on: (c copyFrom: cb stringIndex to: c size).
			readLimit := position := collection size.
			^ self endEntry].
	self display
]

{ #category : #initialization }
Transcripter >> initInFrame: rect [
	frame := rect.
	para := Paragraph new.
	self compose
]

{ #category : #'command line' }
Transcripter >> nextChar [
	^ self class nextChar
]

{ #category : #'command line' }
Transcripter >> readEvalPrint [
	| line okToRevert |
	okToRevert := true.
	Sensor flushKeyboard.
	[#('quit' 'exit' 'done' ) includes: (line := self request: '> ')]
		whileFalse: [line = 'revert'
				ifTrue: [okToRevert
						ifTrue: [(self confirm: 'Revert: ' , RecentMessageList uniqueInstance lastEntry asString , ' ?')
								ifTrue: [RecentMessageList uniqueInstance revertLastMethodSubmission.
									self cr; show: 'reverted: ' , RecentMessageList uniqueInstance lastEntry asString.
									okToRevert := false]]
						ifFalse: [self cr; show: 'Only one level of revert currently supported']]
				ifFalse: [self cr;
						show: ([self class compiler evaluate: line]
								ifError: [:err | err])]]. 
	self cr; show: 'Done'.

]

{ #category : #'command line' }
Transcripter >> request: prompt [ 
	| startPos char contents |
	self cr; show: prompt.
	startPos := position.
	Sensor flushKeyboard.
	[(char := self nextChar) = Character cr]
		whileFalse: [char = Character backspace
				ifTrue: [readLimit := position := position - 1 max: startPos]
				ifFalse: [self nextPut: char].
			self endEntry].
	contents := self contents.
	^ contents copyFrom: startPos + 1 to: contents size 
]

{ #category : #accessing }
Transcripter >> show: anObject [
	self nextPutAll: anObject asString; endEntry
]

{ #category : #private }
Transcripter >> textStyle [
	^ TextStyle default
]

{ #category : #private }
Transcripter >> white [
	Display depth = 1 ifTrue: [^ Bitmap with: 0 "Works without color support"].
	^ Color white
]
