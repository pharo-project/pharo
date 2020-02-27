"
My instances provide space-efficient storage of data which tends to be constant over long runs of the possible indices. Essentially repeated values are stored singly and then associated with a ""run"" length that denotes the number of consecutive occurrences of the value.

My two important variables are
	runs	An array of how many elements are in each run
	values	An array of what the value is over those elements

The variables lastIndex, lastRun and lastOffset cache the last access
so that streaming through RunArrays is not an N-squared process.

Many complexities of access can be bypassed by using the method
	RunArray withStartStopAndValueDo:
"
Class {
	#name : #RunArray,
	#superclass : #SequenceableCollection,
	#instVars : [
		'runs',
		'values',
		'lastIndex',
		'lastRun',
		'lastOffset'
	],
	#category : #'Text-Core-Utilities'
}

{ #category : #'instance creation' }
RunArray class >> new [

	^self runs: Array new values: Array new
]

{ #category : #'instance creation' }
RunArray class >> new: aSize [
	^ self new: aSize withAll: nil
]

{ #category : #'instance creation' }
RunArray class >> new: size withAll: value [ 
	"Answer a new instance of me, whose every element is equal to the
	argument, value."

	size = 0 ifTrue: [^self new].
	^self runs: (Array with: size) values: (Array with: value)
]

{ #category : #'instance creation' }
RunArray class >> newFrom: aCollection [ 
	"Answer an instance of me containing the same elements as aCollection."

	| runs values lastRun lastValue |
	runs := (Array new: aCollection size // 2) writeStream.
	values := (Array new: aCollection size // 2) writeStream.
	lastRun := 0.
	lastValue := Object new.
	aCollection do: [:x | 
		lastValue = x
			ifTrue: [lastRun := lastRun + 1]
			ifFalse:
				[lastRun > 0
					ifTrue:
						[runs nextPut: lastRun.
						values nextPut: lastValue].
				lastRun := 1.
				lastValue := x]].
	lastRun > 0
		ifTrue:
			[runs nextPut: lastRun.
			values nextPut: lastValue].
	^self basicNew setRuns: runs contents setValues: values contents

"	RunArray newFrom: {1. 2. 2. 3}
	{1. $a. $a. 3} as: RunArray
	({1. $a. $a. 3} as: RunArray) values
"
]

{ #category : #'instance creation' }
RunArray class >> readFrom: aStream [
	"Answer an instance of me as described on the stream, aStream."

	| size runs values |
	size := aStream nextWord.
	runs := Array new: size.
	values := Array new: size.
	1 to: size do:
		[:x |
		runs at: x put: aStream nextWord.
		values at: x put: aStream nextWord].
	^ self runs: runs values: values
]

{ #category : #'instance creation' }
RunArray class >> runs: newRuns values: newValues [ 
	"Answer an instance of me with runs and values specified by the 
	arguments."

	| instance |
	instance := self basicNew.
	instance setRuns: newRuns setValues: newValues.
	^instance
]

{ #category : #'instance creation' }
RunArray class >> scanFrom: strm [
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
			(self environment at: #StrikeFont ifPresent: [:sf | sf familyName: (strm upTo: $#) size: (Integer readFrom: strm ifFail: [0])]))].
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
]

{ #category : #copying }
RunArray >> , aRunArray [ 
	"Answer a new RunArray that is a concatenation of the receiver and
	aRunArray."

	| new newRuns |
	(aRunArray isMemberOf: RunArray)
		ifFalse: 
			[new := self copy.
			"attempt to be sociable"
			aRunArray do: [:each | new addLast: each].
			^new].
	runs size = 0 ifTrue: [^aRunArray copy].
	aRunArray runs size = 0 ifTrue: [^self copy].
	(values at: values size) ~= (aRunArray values at: 1)
		ifTrue: [^RunArray
					runs: runs , aRunArray runs
					values: values , aRunArray values].
	newRuns := runs
			copyReplaceFrom: runs size
			to: runs size
			with: aRunArray runs.
	newRuns at: runs size put: (runs at: runs size) + (aRunArray runs at: 1).
	^RunArray
		runs: newRuns
		values: 
			(values
				copyReplaceFrom: values size
				to: values size
				with: aRunArray values)
]

{ #category : #comparing }
RunArray >> = otherArray [ 
	"Test if all my elements are equal to those of otherArray"

	self == otherArray ifTrue: [^true].
	self species == otherArray species ifFalse: [^ false].
	(otherArray isMemberOf: RunArray) ifFalse: [^ self hasEqualElements: otherArray].

	"Faster test between two RunArrays"
	^ (runs hasEqualElements: otherArray runs)
		and: [values hasEqualElements: otherArray values]
]

{ #category : #adding }
RunArray >> add: newObject withOccurrences: anInteger [
	"Add value as the last element of the receiver, the given number of times"
	anInteger <= 0 ifTrue: [ ^self ].
	lastIndex := nil. "flush access cache"
	(runs size=0 or: [values last ~= newObject])
		ifTrue:
			[runs := runs copyWith: anInteger.
			values := values copyWith: newObject]
		ifFalse:
			[runs at: runs size put: runs last + anInteger]
]

{ #category : #adding }
RunArray >> addFirst: value [
	"Add value as the first element of the receiver."
	lastIndex := nil. "flush access cache"
	(runs size=0 or: [values first ~= value])
		ifTrue:
			[runs := {1}, runs.
			values := {value}, values]
		ifFalse:
			[runs at: 1 put: runs first+1]
]

{ #category : #adding }
RunArray >> addLast: value [
	"Add value as the last element of the receiver."
	lastIndex := nil. "flush access cache"
	(runs size=0 or: [values last ~= value])
		ifTrue:
			[runs := runs copyWith: 1.
			values := values copyWith: value]
		ifFalse:
			[runs at: runs size put: runs last+1].
	^value
]

{ #category : #enumerating }
RunArray >> allSatisfy: aBlock [ 
	"Only evaluate once per run"

	^values allSatisfy: aBlock
]

{ #category : #enumerating }
RunArray >> anySatisfy: aBlock [ 
	"Only evaluate once per run"

	^values anySatisfy: aBlock
]

{ #category : #converting }
RunArray >> asBag [
	| aBag |
	aBag := Bag new: values size.
	self runsAndValuesDo: [:run :value |
		aBag add: value withOccurrences: run].
	^aBag
]

{ #category : #converting }
RunArray >> asSet [
	^values asSet
]

{ #category : #accessing }
RunArray >> at: index [

	self at: index setRunOffsetAndValue: [:run :offset :value | ^value]
]

{ #category : #accessing }
RunArray >> at: index put: aValue [
	"Set an element of the RunArray"

	| runIndex offsetInRun lastValue runLength runReplacement valueReplacement iStart iStop |
	index isInteger
		ifFalse: [ self errorNonIntegerIndex ].
	(index between: 1 and: self size)
		ifFalse: [ self errorSubscriptBounds: index ].
	self
		at: index
		setRunOffsetAndValue: [ :run :offset :value | 
			runIndex := run.
			offsetInRun := offset.
			lastValue := value ].
	aValue = lastValue
		ifTrue: [ ^ aValue ].
	runLength := runs at: runIndex.
	runReplacement := Array with: offsetInRun with: 1 with: runLength - offsetInRun - 1.
	valueReplacement := Array with: lastValue with: aValue with: lastValue.
	iStart := offsetInRun = 0
		ifTrue: [ 2 ]
		ifFalse: [ 1 ].
	iStop := offsetInRun = (runLength - 1)
		ifTrue: [ 2 ]
		ifFalse: [ 3 ].
	self
		setRuns: (runs copyReplaceFrom: runIndex to: runIndex with: (runReplacement copyFrom: iStart to: iStop))
		setValues: (values copyReplaceFrom: runIndex to: runIndex with: (valueReplacement copyFrom: iStart to: iStop)).
	self coalesce.
	^ aValue
]

{ #category : #private }
RunArray >> at: index setRunOffsetAndValue: aBlock [ 
	"Supply all run information to aBlock."
	"Tolerates index=0 and index=size+1 for copyReplace: "
	| run limit offset |
	limit := runs size.
	(lastIndex == nil or: [index < lastIndex])
		ifTrue: "cache not loaded, or beyond index - start over"
			[run := 1.
			offset := index-1]
		ifFalse: "cache loaded and before index - start at cache"
			[run := lastRun.
			offset := lastOffset + (index-lastIndex)].
	[run <= limit and: [offset >= (runs at: run)]]
		whileTrue: 
			[offset := offset - (runs at: run).
			run := run + 1].
	lastIndex := index. "Load cache for next access"
	lastRun := run.
	lastOffset := offset.
	run > limit
		ifTrue: 
			["adjustment for size+1"
			run := run - 1.
			offset := offset + (runs at: run)].
	^aBlock
		value: run "an index into runs and values"
		value: offset "zero-based offset from beginning of this run"
		value: (values at: run) "value for this run"
]

{ #category : #adding }
RunArray >> coalesce [
	"Try to combine adjacent runs"
	| ind |
	ind := 2.
	[ind > values size] whileFalse: [
		(values at: ind-1) = (values at: ind) 
			ifFalse: [ind := ind + 1]
			ifTrue: ["two are the same, combine them"
				values := values copyReplaceFrom: ind to: ind with: #().
				runs at: ind-1 put: (runs at: ind-1) + (runs at: ind).
				runs := runs copyReplaceFrom: ind to: ind with: #().
				"self error: 'needed to combine runs' "]].
			
]

{ #category : #enumerating }
RunArray >> collect: aBlock [
	"Beware, the block will be evaluated only once per group of values."
	^(self class runs: runs copy contents values: (values collect: aBlock)) coalesce
]

{ #category : #copying }
RunArray >> copyFrom: start to: stop [
	| newRuns run1 run2 offset1 offset2 | 
	stop < start ifTrue: [^RunArray new].
	self at: start setRunOffsetAndValue: [:r :o :value1 | run1 := r. offset1 := o. value1].
	self at: stop setRunOffsetAndValue: [:r :o :value2 | run2 := r. offset2 := o. value2].
	run1 = run2
		ifTrue: 
			[newRuns := Array with: offset2 - offset1 + 1]
		ifFalse: 
			[newRuns := runs copyFrom: run1 to: run2.
			newRuns at: 1 put: (newRuns at: 1) - offset1.
			newRuns at: newRuns size put: offset2 + 1].
	^RunArray runs: newRuns values: (values copyFrom: run1 to: run2)
]

{ #category : #copying }
RunArray >> copyReplaceFrom: start to: stop with: replacement [

	^(self copyFrom: 1 to: start - 1)
		, replacement 
		, (self copyFrom: stop + 1 to: self size)
]

{ #category : #copying }
RunArray >> copyUpThrough: anElement [
	"Optimized"

	| newValues |
	newValues := values copyUpThrough: anElement.
	^ self class
		runs: (runs copyFrom: 1 to: newValues size)
		values: newValues
]

{ #category : #copying }
RunArray >> copyUpTo: anElement [ 
	"Optimized"

	| newValues |
	newValues := values copyUpTo: anElement.
	^ self class
		runs: (runs copyFrom: 1 to: newValues size)
		values: newValues
]

{ #category : #copying }
RunArray >> copyUpToLast: anElement [
	"Optimized"

	| newValues |
	newValues := values copyUpToLast: anElement.
	^ self class
		runs: (runs copyFrom: 1 to: newValues size)
		values: newValues
]

{ #category : #enumerating }
RunArray >> count: aBlock [
	"Beware, the block will be evaluated only once per group of values."
	| count |
	count := 0.
	self runsAndValuesDo: [:run :value |
		(aBlock value: value)
			ifTrue:
				[count := count + run]].
	^count
]

{ #category : #enumerating }
RunArray >> detect: aBlock ifNone: exceptionBlock [ 
	"Only evaluate once per run"

	^values detect: aBlock ifNone: exceptionBlock 
]

{ #category : #enumerating }
RunArray >> detectMax: aBlock [
	"Only evaluate once per run"

	^values detectMax: aBlock
]

{ #category : #enumerating }
RunArray >> detectMin: aBlock [
	"Only evaluate once per run"

	^values detectMin: aBlock
]

{ #category : #enumerating }
RunArray >> detectSum: aBlock [
	"Only loop on runs"
	| sum |
	sum := 0.
	self runsAndValuesDo: [:run :value | 
		sum := (aBlock value: value) * run + sum].
	^ sum
]

{ #category : #enumerating }
RunArray >> do: aBlock [
	"This is refined for speed"
	
	1 to: runs size do: [:i |
		| r v |
		v := values at: i.
		r := runs at: i.
		[( r := r - 1) >= 0]
			whileTrue: [aBlock value: v]].
]

{ #category : #private }
RunArray >> fillFrom: aCollection with: aBlock [
	"Evaluate aBlock with each of aCollection's elements as the argument.
	Collect the resulting values into self. Answer self."

	| newRuns newValues lastLength lastValue |
	newRuns := (Array new: aCollection size) writeStream.
	newValues := (Array new: aCollection size) writeStream.
	lastLength := 0.
	lastValue := Object new.
	aCollection do: [:each | 
		| value |
		value := aBlock value: each.
		lastValue = value
			ifTrue: [lastLength := lastLength + 1]
			ifFalse:
				[lastLength > 0
					ifTrue:
						[newRuns nextPut: lastLength.
						newValues nextPut: lastValue].
				lastLength := 1.
				lastValue := value]].
	lastLength > 0
		ifTrue:
			[newRuns nextPut: lastLength.
			newValues nextPut: lastValue].
	self setRuns: newRuns contents setValues: newValues contents
]

{ #category : #enumerating }
RunArray >> findFirst: aBlock [
	| index |
	index := 1.
	self runsAndValuesDo: [ :run :value |
		(aBlock value: value) ifTrue: [^index].
		index := index + run].
	^0
]

{ #category : #enumerating }
RunArray >> findLast: aBlock [
	| index |
	index := values size + 1.
	[(index := index - 1) >= 1] whileTrue:
		[(aBlock value: (values at: index)) ifTrue: [^(1 to: index) detectSum: [:i | runs at: i]]].
	^0
]

{ #category : #accessing }
RunArray >> first [
	^values at: 1
]

{ #category : #testing }
RunArray >> includes: anObject [
	"Answer whether anObject is one of the receiver's elements."

	^values includes: anObject
]

{ #category : #'self evaluating' }
RunArray >> isSelfEvaluating [
	^ self class == RunArray and: [values isSelfEvaluating]
]

{ #category : #testing }
RunArray >> isSorted [
	^values isSorted
]

{ #category : #testing }
RunArray >> isSortedBy: aBlock [
	^values isSortedBy: aBlock
]

{ #category : #accessing }
RunArray >> last [
	^values at: values size
]

{ #category : #private }
RunArray >> mapValues: mapBlock [
	"NOTE: only meaningful to an entire set of runs"

	values := values collect: mapBlock
]

{ #category : #enumerating }
RunArray >> noneSatisfy: aBlock [ 
	"Only evaluate once per run"

	^values noneSatisfy: aBlock
]

{ #category : #copying }
RunArray >> postCopy [
	super postCopy.
	runs := runs copy.
	values := values copy
]

{ #category : #printing }
RunArray >> printOn: aStream [
	self printNameOn: aStream.
	aStream
		nextPutAll: ' runs: ';
		print: runs;
		nextPutAll: ' values: ';
		print: values
]

{ #category : #adding }
RunArray >> rangeOf: attr startingAt: startPos [
	"Answer an interval that gives the range of attr at index position startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos. self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self.
Note that an attribute may span several adjancent runs. "

	self at: startPos 
		setRunOffsetAndValue: 
			[:run :offset :value | 
			^(value includes: attr)
				ifFalse: [startPos to: startPos - 1]
				ifTrue:
					[ | firstRelevantPosition lastRelevantPosition idxOfCandidateRun |
					lastRelevantPosition := startPos - offset + (runs at: run) - 1.
					firstRelevantPosition := startPos - offset.
					idxOfCandidateRun := run + 1.
					[idxOfCandidateRun <= runs size 
							 and: [(values at: idxOfCandidateRun) includes: attr]]
						whileTrue:
							[lastRelevantPosition := lastRelevantPosition + (runs at: idxOfCandidateRun).
							idxOfCandidateRun := idxOfCandidateRun + 1]. 
					idxOfCandidateRun := run - 1.
					[idxOfCandidateRun >= 1 
							 and: [(values at: idxOfCandidateRun) includes: attr]]
						whileTrue:
							[firstRelevantPosition := firstRelevantPosition - (runs at: idxOfCandidateRun).
							idxOfCandidateRun := idxOfCandidateRun - 1]. 
 
					firstRelevantPosition to: lastRelevantPosition]]
]

{ #category : #removing }
RunArray >> remove: anObject ifAbsent: exceptionBlock [
	| index mustCoalesce run |
	index := values indexOf: anObject ifAbsent: [^exceptionBlock value].
	(run := runs at: index) > 1
		ifTrue: [runs at: index put: run - 1]
		ifFalse:
			[mustCoalesce := index > 1 and: [index < values size and: [(values at: index - 1) = (values at: index + 1)]].
			runs := runs copyWithoutIndex: index.
			values := values copyWithoutIndex: index.
			mustCoalesce
				ifTrue:
					[runs at: index - 1 put: (runs at: index - 1) + (runs at: index).
					runs := runs copyWithoutIndex: index.
					values := values copyWithoutIndex: index]].
	^anObject
]

{ #category : #removing }
RunArray >> removeAll [
	runs := Array new.
	values := Array new
]

{ #category : #adding }
RunArray >> repeatLast: times ifEmpty: defaultBlock [
	"add the last value back again, the given number of times. If we are empty, add (defaultBlock value)"
	times = 0 ifTrue: [^self ].
	lastIndex := nil. "flush access cache"
	(runs size=0)
		ifTrue:
			[runs := runs copyWith: times.
			values := values copyWith: defaultBlock value]
		ifFalse:
			[runs at: runs size put: runs last+times] 
]

{ #category : #adding }
RunArray >> repeatLastIfEmpty: defaultBlock [
	"add the last value back again. If we are empty, add (defaultBlock value)"
	lastIndex := nil. "flush access cache"
	(runs size=0)
		ifTrue:[
			runs := runs copyWith: 1.
			values := values copyWith: defaultBlock value]
		ifFalse:
			[runs at: runs size put: runs last+1]
]

{ #category : #enumerating }
RunArray >> replace: aBlock [
	"Beware, the block will be evaluated only once per group of values."
	values := values replace: aBlock.
	self coalesce
]

{ #category : #enumerating }
RunArray >> reverseDo: aBlock [
	"This is refined for speed"
	
	| i |
	i := runs size.
	[i > 0]
		whileTrue: 
			[ | r v |
			v := values at: i.
			r := runs at: i.
			i := i - 1.
			[( r := r - 1) >= 0]
				whileTrue: [aBlock value: v]].
]

{ #category : #converting }
RunArray >> reversed [
	^self class runs: runs reversed values: values reversed
]

{ #category : #accessing }
RunArray >> runLengthAt: index [ 
	"Answer the length remaining in run beginning at index."

	self at: index 
		setRunOffsetAndValue: [:run :offset :value | ^(runs at: run) - offset]
]

{ #category : #private }
RunArray >> runs [

	^runs
]

{ #category : #enumerating }
RunArray >> runsAndValuesDo: aBlock [
	"Evaluate aBlock with run lengths and values from the receiver"
	^runs with: values do: aBlock.
]

{ #category : #enumerating }
RunArray >> runsFrom: start to: stop do: aBlock [
	"Evaluate aBlock with all existing runs in the range from start to stop"
	start > stop ifTrue:[^self].
	self at: start setRunOffsetAndValue:[:firstRun :offset :firstValue|
		| run value index |
		run := firstRun.
		value := firstValue.
		index := start + (runs at: run) - offset.
		[aBlock value: value.
		index <= stop] whileTrue:[
			run := run + 1.
			value := values at: run.
			index := index + (runs at: run)]].

]

{ #category : #enumerating }
RunArray >> select: aBlock [
	"Beware, the block will be evaluated only once per group of values."
	| newRuns newValues |
	newRuns := (Array new: runs size) writeStream.
	newValues := (Array new: values size) writeStream.
	self runsAndValuesDo: [:run :value |
		(aBlock value: value)
			ifTrue:
				[newRuns add: run.
				newValues add: value]].
	^(self class runs: newRuns contents values: newValues contents) coalesce
]

{ #category : #private }
RunArray >> setRuns: newRuns setValues: newValues [
	lastIndex := nil. "flush access cache"
	runs := newRuns asArray.
	values := newValues asArray.
]

{ #category : #accessing }
RunArray >> size [
	| size |
	size := 0.
	1 to: runs size do: [:i | size := size + (runs at: i)].
	^size
]

{ #category : #printing }
RunArray >> storeOn: aStream [

	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' runs: '.
	runs storeOn: aStream.
	aStream nextPutAll: ' values: '.
	values storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #private }
RunArray >> values [
	"Answer the values in the receiver."

	^values
]

{ #category : #accessing }
RunArray >> withStartStopAndValueDo: aBlock [
	| start |
	start := 1.
	runs with: values do:
		[:len : val | | stop |
		stop := start + len - 1.
		aBlock value: start value: stop value: val.
		start := stop + 1]
		
]

{ #category : #printing }
RunArray >> writeOn: aStream [

	aStream nextWordPut: runs size.
	1 to: runs size do:
		[:x |
		aStream nextWordPut: (runs at: x).
		aStream nextWordPut: (values at: x)]
]
