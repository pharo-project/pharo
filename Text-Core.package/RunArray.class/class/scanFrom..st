instance creation
scanFrom: strm
	"Read the style section of a fileOut or sources file.	nextChunk has already been done. We need to return a RunArray of TextAttributes of various kinds. These are written by the implementors of writeScanOn:"
	| runs values attrList char |
	(strm peekFor: $( ) ifFalse: [^ nil].
	runs := OrderedCollection new.
	[strm skipSeparators.
	 strm peekFor: $)] whileFalse: 
		[runs add: (Number readFrom: strm)].
	values := OrderedCollection new. "Value array"
	attrList := OrderedCollection new. "Attributes list"
	[(char := strm next) == nil] whileFalse: [
		char == $, ifTrue: [values add: attrList asArray. attrList := OrderedCollection new].
		char == $a ifTrue: [attrList add: 
			(TextAlignment new alignment: (Integer readFrom: strm ifFail: [0]))].
		char == $f ifTrue: [attrList add: 
			(TextFontChange new fontNumber: (Integer readFrom: strm ifFail: [0]))].
		char == $F ifTrue: [attrList add: (TextFontReference toFont: 
			(Smalltalk at: #StrikeFont ifPresent: [:sf | sf familyName: (strm upTo: $#) size: (Integer readFrom: strm ifFail: [0])]))].
		char == $b ifTrue: [attrList add: (TextEmphasis bold)].
		char == $i ifTrue: [attrList add: (TextEmphasis italic)].
		char == $u ifTrue: [attrList add: (TextEmphasis underlined)].
		char == $= ifTrue: [attrList add: (TextEmphasis struckOut)].
		char == $n ifTrue: [attrList add: (TextEmphasis normal)].
		char == $- ifTrue: [attrList add: (TextKern kern: -1)].
		char == $+ ifTrue: [attrList add: (TextKern kern: 1)].
		char == $c ifTrue: [attrList add: (TextColor scanFrom: strm)]. "color"
		char == $L ifTrue: [attrList add: (TextLink scanFrom: strm)]. "L not look like 1"
		char == $R ifTrue: [attrList add: (TextURL scanFrom: strm)].
				"R capitalized so it can follow a number"
		char == $P ifTrue: [attrList add: (TextPrintIt scanFrom: strm)].
		char == $d ifTrue: [attrList add: (TextDoIt scanFrom: strm)].
		"space, cr do nothing"
		].
	values add: attrList asArray.
	^ self runs: runs asArray values: (values copyFrom: 1 to: runs size) asArray
"
RunArray scanFrom: (ReadStream on: '(14 50 312)f1,f1b,f1LInteger +;i')
"