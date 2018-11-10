"
I'm a scanner to get correct sources from method definitions.
Is important to follow this remarks:

Since a method body can contain enclosing brackets we need to be sure we will skip them and
	 correctly read the method. For that, I have to take into account: 
		- I can mention [] in comments
		- I can mention [] in strings
		- I can mention [] in array literals
		- I can use $[, $] 
		- I can have inner blocks
		- I can mention a comment of the form """"$"""" or a comment of the form '$'
	 all that needs to be skipped 
"
Class {
	#name : #TonelSourceScanner,
	#superclass : #TonelScanner,
	#instVars : [
		'isComment',
		'isString',
		'isLiteralArray',
		'char',
		'prevChar',
		'prevPrevChar',
		'result',
		'count',
		'isFinished'
	],
	#category : #'MonticelloTonel-Core'
}

{ #category : #testing }
TonelSourceScanner >> isStartingComment [
	"I'm starting a comment if char is $"" and previous char is not a dollar (which would mark the 
	 character $"". I have to take into account too that if previous character is dollar, it can be 
	 also the character dollar $$. 
	 So, I accept "" and also $$"", but I reject $""  
	"
	^ char = $" and: [ prevChar ~= $$ or: [ prevPrevChar == $$ ] ]
]

{ #category : #testing }
TonelSourceScanner >> isStartingLiteralArray [
	"I accept if character is $( and is preceded by $#"
	^ char = $( and: [ prevChar = $# ]
]

{ #category : #testing }
TonelSourceScanner >> isStartingString [
	"I accept if character is $' but is not preceded by a $$"
	^ char = $' and: [ prevChar ~= $$ ]
]

{ #category : #private }
TonelSourceScanner >> prepareToScan [
	result := String new writeStream.
	isFinished := isComment := isString := isLiteralArray := false.
	char := prevChar := prevPrevChar := nil.
	count := 0
	
]

{ #category : #private }
TonelSourceScanner >> readNext [
	prevPrevChar := prevChar.
	prevChar := char.
	result nextPut: (char := stream next)

]

{ #category : #private }
TonelSourceScanner >> readUntil: aBlock [
	[ stream atEnd ]
	whileFalse: [
		self readNext.
		aBlock value ifTrue: [ ^ self ] ]
]

{ #category : #private }
TonelSourceScanner >> readUntilTermination: matchChar [
	| terminationCount |
	"This is used to read sequences that start and finish with same character termination. 
	 tipically, this is for strings and comments, but who knows what comes in the future."
	terminationCount := 1.
	self readUntil: [ 
		char = matchChar ifTrue: [ terminationCount := terminationCount + 1 ].
		terminationCount even and: [ stream peek ~= matchChar ] ]
]

{ #category : #scanning }
TonelSourceScanner >> scan [
	self prepareToScan.	
	stream peek = $[ ifFalse: [ TonelParseError signal: 'Can''t parse method body' ].
	[ stream atEnd or: [ isFinished ] ]
	whileFalse: [ self scanNextChunk ].
	isFinished ifTrue: [ 
		"clean up to return"
		^ self 
			removeFrom: result contents withSqueakLineEndings
			enclosingStart: $[ 
			end: $]
			clean: #right ].

	TonelParseError signal: 'Can''t parse method body'
]

{ #category : #scanning }
TonelSourceScanner >> scanForBlockIfMatch: aBlock [
	| match |
	
	match := false.
	(char = $[ and: [  prevChar ~= $$ ]) ifTrue: [ 
		count := count + 1.
		match := true ].
	(char = $] and: [ prevChar ~= $$ ]) ifTrue: [ 
		count := count - 1.
		match := true ].
	count = 0 ifTrue: [ isFinished := true ].
	match ifTrue: aBlock

]

{ #category : #scanning }
TonelSourceScanner >> scanForCommentIfMatch: aBlock [
	self isStartingComment ifFalse: [ ^ self ]. 
	self readUntilTermination: $".
	aBlock value.
]

{ #category : #scanning }
TonelSourceScanner >> scanForLiteralArrayIfMatch: aBlock [
	| literalCount |

	self isStartingLiteralArray ifFalse: [ ^ self ]. 
	literalCount := 1.
	self readUntil: [  
		self scanForStringIfMatch: [].
		self scanForCommentIfMatch: [].
		char = $( ifTrue: [ literalCount := literalCount + 1 ].
		char = $) ifTrue: [ literalCount := literalCount - 1 ].
		literalCount = 0 ].
	aBlock value
]

{ #category : #scanning }
TonelSourceScanner >> scanForStringIfMatch: aBlock [
	self isStartingString ifFalse: [ ^ self ].
	self readUntilTermination: $'.
	aBlock value.
]

{ #category : #scanning }
TonelSourceScanner >> scanNextChunk [
	self readNext.	
	self scanForBlockIfMatch: [ ^ self ].
	self scanForCommentIfMatch: [ ^ self ].
	self scanForStringIfMatch: [ ^ self ].
	self scanForLiteralArrayIfMatch: [ ^ self ]
]
