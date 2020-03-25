"
I implement the diff algorithm. I can show the differences between two texts. See my method comments for further information.

Instance Variables
	xLines:		<Array>
	yLines:		<Array>

xLines
	- an Array of DiffElement which is created from the first input text

yLines
	- an Array of DiffElement which is created from the second input text
"
Class {
	#name : #TextDiffBuilder,
	#superclass : #Object,
	#instVars : [
		'xLines',
		'yLines'
	],
	#classVars : [
		'DiffsWithPrettyPrint',
		'IgnoreLineEndings',
		'InsertTextAttributes',
		'NormalTextAttributes',
		'RemoveTextAttributes'
	],
	#category : #'Text-Diff-Building'
}

{ #category : #'instance creation' }
TextDiffBuilder class >> buildDisplayPatchFrom: sourceText to: destinationText [

	^(self from: sourceText to: destinationText) buildDisplayPatch
]

{ #category : #'instance creation' }
TextDiffBuilder class >> buildDisplayPatchFrom: sourceText to: destinationText inClass: sourceClass [ 

	^self 
		buildDisplayPatchFrom: sourceText 
		to: destinationText
		inClass: sourceClass
		prettyDiffs: self diffsWithPrettyPrint
		
]

{ #category : #'instance creation' }
TextDiffBuilder class >> buildDisplayPatchFrom: srcString to: dstString inClass: srcClass prettyDiffs: prettyDiffs [ 
	"Build a display patch for mapping via diffs from srcString to dstString
	in the given class. If prettyDiffs is true, do the diffing for pretty-printed forms"

	| differ |
	differ := prettyDiffs
				ifTrue: [PrettyTextDiffBuilder]
				ifFalse: [self].
	^ (differ
		from: srcString
		to: dstString
		inClass: srcClass) buildDisplayPatch
]

{ #category : #settings }
TextDiffBuilder class >> differatorSettingsOn: aBuilder [ 
	<systemsettings>
	(aBuilder group: #differator) 
		label: 'Code Diffs';
		parent: #codeBrowsing; 
		description: 'All settings concerned with the computation of diffs' ;
		with: [	
			(aBuilder setting: #ignoreLineEndings) 
				label: 'Ignore line endings';
				target: self;
				default: true;
		 		description: 'When selected, line ending differences will be ignored ' ];
		with: [  
		(aBuilder setting: #diffsWithPrettyPrint)
				target: self;	
				default: false;
				label: 'Pretty print differences' ;
				description: 'If true, displays of source code differences will be pretty-printed first' 
		]
]

{ #category : #settings }
TextDiffBuilder class >> diffsWithPrettyPrint [
	^ DiffsWithPrettyPrint ifNil: [DiffsWithPrettyPrint := false]
]

{ #category : #settings }
TextDiffBuilder class >> diffsWithPrettyPrint: aBoolean [
	DiffsWithPrettyPrint := aBoolean
]

{ #category : #'instance creation' }
TextDiffBuilder class >> from: srcString to: dstString [

	^self new from: srcString to: dstString
]

{ #category : #'instance creation' }
TextDiffBuilder class >> from: srcString to: dstString inClass: srcClass [
	"srcClass not used when not pretty printing, but have it for polymorphism with PrettyTextDiffBuilder"

	^self new from: srcString to: dstString
]

{ #category : #settings }
TextDiffBuilder class >> ignoreLineEndings [
	"Answer a boolean telling if line endings differences should be ignored or emphasized"
	
	^IgnoreLineEndings ifNil: [ true ]
]

{ #category : #settings }
TextDiffBuilder class >> ignoreLineEndings: aBoolean [
	"Setting for  if line ending differences should be ignored or emphasized"
	
	IgnoreLineEndings := aBoolean
]

{ #category : #'class initialization' }
TextDiffBuilder class >> initialize [

	self initializeTextAttributes
]

{ #category : #'class initialization' }
TextDiffBuilder class >> initializeTextAttributes [

	InsertTextAttributes :=  {TextEmphasis bold. TextColor color: Color green muchDarker}.
	RemoveTextAttributes := {TextEmphasis bold. TextColor color: Color red darker} .
	NormalTextAttributes :={ TextEmphasis normal }
]

{ #category : #'creating patches' }
TextDiffBuilder >> buildDisplayPatch [

	^Text streamContents: [ :stream |
		self 
			patchSequenceDoIfMatch: [ :string |
				self print: string withAttributes: NormalTextAttributes on: stream ]
			ifInsert: [ :string |
				self print: string withAttributes: InsertTextAttributes on: stream ]
			ifRemove: [ :string |
				self print: string withAttributes: RemoveTextAttributes on: stream ] ]
]

{ #category : #'creating patches' }
TextDiffBuilder >> buildPatchSequence [
	"This method is only implemented for backwards compatibility and testing."

	^Array streamContents: [ :stream |
		self 
			patchSequenceDoIfMatch: [ :string |
				stream nextPut: #match -> string ]
			ifInsert: [ :string | 
				stream nextPut: #insert -> string ]
			ifRemove: [ :string | 
				stream nextPut: #remove -> string ] ]
]

{ #category : #private }
TextDiffBuilder >> findMatches [
	"I find the matching pairs of xLines and yLines. First I filter out all lines that can't have a pair, then I find the longest common subsequence of the remaining elements. Finally I mark the matching pairs."

	| temp lcs xFilteredLines yFilteredLines xNumbers yNumbers |
	"Filter out all lines that can't have a pair."
	temp := yLines asSet.
	xFilteredLines := xLines select: [ :each | temp includes: each ].
	xFilteredLines size = 0
		ifTrue: [ ^ self ].
	temp := xLines asSet.
	yFilteredLines := yLines select: [ :each | temp includes: each ].
	yFilteredLines size = 0
		ifTrue: [ ^ self ].	"Map all lines to SmallIntegers, because they can be compared faster."
	temp := Dictionary new.
	xNumbers := xFilteredLines collect: [ :each | temp at: each ifAbsentPut: [ temp size ] ].
	yNumbers := yFilteredLines collect: [ :each | temp at: each ifAbsentPut: [ temp size ] ].
	temp := nil.	"Find the longest common subsequence."
	lcs := self lcsFor: xNumbers and: yNumbers.	"Mark the matching pairs."
	[ lcs isNil ]
		whileFalse: [ 
			(xFilteredLines at: (lcs at: 1)) matches: (yFilteredLines at: (lcs at: 2)).
			lcs := lcs at: 3 ]
]

{ #category : #initialize }
TextDiffBuilder >> from: xString to: yString [

	xLines := (self split: xString asString) replace: [ :each | DiffElement string: each ].
	yLines := (self split: yString asString) replace: [ :each | DiffElement string: each ].
	self findMatches
]

{ #category : #private }
TextDiffBuilder >> lcsFor: xFilteredLines and: yFilteredLines [
	"I find one of the longest common subsequences of my the arguments. I assume that none of my arguments are empty. I return nil or an Array which represents a list. The first two elements are the matching line numbers, the last is the next node in the list or nil if there are no more elements. The list containts the longest common subsequence. I'm a modified version of the greedy lcs algorithm from the 6th page of 'An O(ND) Difference Algorithm and Its Variations (1986)' by Eugene W. Myers"

	| n m v lcss max |
	n := xFilteredLines size.
	m := yFilteredLines size.
	max := m + n.
	v := Array new: 2 * max + 1.
	v at: max + 2 put: 0.
	lcss := Array new: 2 * max + 1.
	0 to: max do: [ :d |
		d negated to: d by: 2 do: [ :k |
			| index lcs x y |
			index := max + k.
			(k + d = 0 or: [ k ~= d and: [ (v at: index) < (v at: index + 2) ] ])
				ifTrue: [ x := v at: (index := index + 2) ]
				ifFalse: [ x := (v at: index) + 1 ].
			y := x - k.
			lcs := lcss at: index.
			[ x < n and: [ y < m and: [ (xFilteredLines at: x + 1) = (yFilteredLines at: y + 1) ] ] ]
				whileTrue: [
					lcs := { x := x + 1. y := y + 1. lcs } ].
			(x >= n and: [ y >= m ]) ifTrue: [ ^lcs ].
			v at: max + k + 1 put: x.
			lcss at: max + k + 1 put: lcs ] ].
	self error
]

{ #category : #'creating patches' }
TextDiffBuilder >> patchSequenceDoIfMatch: matchBlock ifInsert: insertBlock ifRemove: removeBlock [
	"I'm the general purpose method to iterate through the patch sequence. See my senders to learn how to use me."

	| xLine xLineStream |
	xLineStream := xLines readStream.
	yLines
		do: [ :yLine | 
			yLine hasMatch
				ifFalse: [ insertBlock value: yLine string ]
				ifTrue: [ 
					[ (xLine := xLineStream next) isNil or: [ xLine == yLine match ] ] whileFalse: [ removeBlock value: xLine string ].
					matchBlock value: yLine string ] ].
	[ (xLine := xLineStream next) isNil ] whileFalse: [ removeBlock value: xLine string ]
]

{ #category : #private }
TextDiffBuilder >> print: aString withAttributes: attributes on: stream [

	stream
		withAttributes: attributes 
		do: [ 
			stream nextPutAll: aString.
			(aString notEmpty and: [ 
				aString last = Character cr or: [
					aString endsWith: String crlf ] ]) 
						ifFalse: [ stream cr ] ]
]

{ #category : #private }
TextDiffBuilder >> split: aString [
	"I return an Array of strings which are the lines extracted from aString. All lines contain the line separator characters, or not depending on preference."

	^Array streamContents: [ :stream |
		self class ignoreLineEndings
			ifTrue: [aString lineIndicesDo: [ :start :endWithoutSeparators :end |
				stream nextPut: (aString copyFrom: start to: endWithoutSeparators) ] ]
			ifFalse: [aString lineIndicesDo: [ :start :endWithoutSeparators :end |
				stream nextPut: (aString copyFrom: start to: end) ] ] ]
]
