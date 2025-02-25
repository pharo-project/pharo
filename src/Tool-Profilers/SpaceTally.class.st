"
I'm responsible to help getting information about system space usage. The information I compute is represented by a spaceTallyItem

try something like: 

((SpaceTally new spaceTally: (Array with: TextMorph with: Point)) 
	asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) 

SpaceTally new systemWideSpaceTally


This class has been created from a part of SystemEnvironment. It still deserves a nice
clean, such as using object instead of array having 4 slots.

sd-20 June 2003
"
Class {
	#name : 'SpaceTally',
	#superclass : 'Object',
	#instVars : [
		'results'
	],
	#category : 'Tool-Profilers-Space',
	#package : 'Tool-Profilers',
	#tag : 'Space'
}

{ #category : 'fileout' }
SpaceTally class >> printSpaceAnalysis [
	<script>
	self new printSpaceAnalysis
]

{ #category : 'fileout' }
SpaceTally class >> printSpaceAnalysis: aThreshold on: aStream [
	<script>
	self new
		systemWideSpaceTally;
		printSpaceAnalysis: aThreshold on: aStream
]

{ #category : 'class analysis' }
SpaceTally >> compareTallyIn: beforeFileName to: afterFileName [
	"SpaceTally new compareTallyIn: 'tally' to: 'tally2'"

	| answer beforeDict a afterDict allKeys |
	beforeDict := Dictionary new.
	beforeFileName asFileReference readStreamDo: [ :s|
		[s atEnd] whileFalse: [
			a := Array readFrom: s nextLine.
			beforeDict at: a first put: a allButFirst ]].
	afterDict := Dictionary new.

	afterFileName asFileReference readStreamDo: [ :s|
		[s atEnd] whileFalse: [
			a := Array readFrom: s nextLine.
			afterDict at: a first put: a allButFirst ]].

	answer := String new writeStream.
	allKeys := (Set new addAll: beforeDict keys; addAll: afterDict keys; yourself) asSortedCollection.
	allKeys do: [ :each | | before diff after |
		before := beforeDict at: each ifAbsent: [#(0 0 0)].
		after := afterDict at: each ifAbsent: [#(0 0 0)].
		diff := before with: after collect: [ :vBefore :vAfter | vAfter - vBefore].
		diff = #(0 0 0) ifFalse: [
			answer nextPutAll: each,'  ',diff printString; cr.
		].
	].
	StandardWindow new
		withText: answer contents
		label: 'space diffs';
		openInWorld
]

{ #category : 'fileout' }
SpaceTally >> computeSpaceForClass: cl [
	| codeSpace instSpace instCount |
	codeSpace := cl spaceUsed.
	Smalltalk garbageCollectMost.
	instCount := cl instanceCount.
	instSpace := cl instancesSizeInMemory.
	results
		addItem:
			(SpaceTallyItem
				analyzedClassName: cl name
				codeSize: codeSpace
				instanceCount: instCount
				spaceForInstances: instSpace)
]

{ #category : 'class analysis' }
SpaceTally >> computeSpaceUsage [
	| c instanceCount |
	results items
		do: [ :entry |
			c := self class environment at: entry analyzedClassName.
			instanceCount := c instanceCount.
			entry codeSize: c spaceUsed.
			entry instanceCount: instanceCount.
			entry spaceForInstances: c instancesSizeInMemory.
			Smalltalk garbageCollectMost ]
]

{ #category : 'fileout' }
SpaceTally >> computeSpaceUsageResultsForClassesInstancesAssociations: aCollectionOfClasses [
	results := SpaceTallyResult new: aCollectionOfClasses size.
	aCollectionOfClasses associations
		do: [ :assoc |
			| instances class |
			instances := assoc value.
			class := assoc key.
			class isMeta
				ifFalse: [ results
						addItem:
							(SpaceTallyItem
								analyzedClassName: class name
								codeSize: class spaceUsed
								instanceCount: instances size
								spaceForInstances: (instances sum: #sizeInMemory)) ] ]
]

{ #category : 'class analysis' }
SpaceTally >> preAllocateResultsFor: classes [
	results := SpaceTallyResult new: classes size.
	classes
		do: [ :cl | results addItem: (SpaceTallyItem analyzedClassName: cl name) ]
]

{ #category : 'fileout' }
SpaceTally >> printHeader: aStream [
	aStream
		nextPutAll: ('Class' padRightTo: 45);
		nextPutAll: ('code space' padLeftTo: 12);
		nextPutAll: ('# instances' padLeftTo: 12);
		nextPutAll: ('inst space' padLeftTo: 12);
		nextPutAll: ('percent' padLeftTo: 12);
		nextPutAll: ('inst average size' padLeftTo: 20);
		cr
]

{ #category : 'fileout' }
SpaceTally >> printSpaceAnalysis [
	self systemWideSpaceTally.
	'STspace.txt' asFileReference
		writeStreamDo: [ :stream | self printSpaceAnalysis: 1 on: stream ]
]

{ #category : 'printing' }
SpaceTally >> printSpaceAnalysis: threshold on: aStream [
"SpaceTally new printSpaceAnalysis: 1 on: (FileStream forceNewFileNamed: 'STspace.text')"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| totalPercent items |
	"If inst count threshold > 0, then sort by space"
	totalPercent := 0.0.
	items :=  results itemsForThreshold: threshold.
	self printHeader: aStream.
	items
		do: [ :s |
			| percent |
			percent := s spaceForInstances * 100.0
				/  results totalInstSpace roundTo: 0.1.
			totalPercent := totalPercent + percent.
			self printSpaceTallyItem: s percent: percent on: aStream ].
	aStream
		cr;
		nextPutAll: ('Total' padRightTo: 45);
		nextPutAll: ( results totalCodeSpace printString padLeftTo: 12);
		nextPutAll: ( results totalInstCount printString padLeftTo: 12);
		nextPutAll: ( results totalInstSpace printString padLeftTo: 14);
		nextPutAll: ((totalPercent roundTo: 0.1) printString padLeftTo: 12)
]

{ #category : 'fileout' }
SpaceTally >> printSpaceDifferenceFrom: fileName1 to: fileName2 [
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		SpaceTally new printSpaceAnalysis: 0 on: 'STspace.text1' asFileReference writeStream.
			--- do something that uses space here ---
		SpaceTally new printSpaceAnalysis: 0 on: 'STspace.text2' asFileReference writeStream.
		SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'
"
	| f coll1 coll2 item |
	f := fileName1 asFileReference readStream.
	coll1 := OrderedCollection new.
	[f atEnd] whileFalse: [coll1 add: f nextLine].
	f close.
	f := fileName2 asFileReference readStream.
	coll2 := OrderedCollection new.
	[f atEnd] whileFalse:
		[item := f nextLine.
		((coll1 includes: item) and: [(item endsWith: 'percent') not])
			ifTrue: [coll1 remove: item]
			ifFalse: [coll2 add: item]].
	f close.
	StandardWindow new
		withText: (String streamContents:
			[:s |
			s nextPutAll: fileName1; cr.
			coll1 do: [:x | s nextPutAll: x; cr].
			s cr; cr.
			s nextPutAll: fileName2; cr.
			coll2 do: [:x | s nextPutAll: x; cr]])
		 label: 'Differential Space Analysis';
		openInWorld
]

{ #category : 'fileout' }
SpaceTally >> printSpaceTallyItem: aSpaceTallyItem percent: percent on: aStream [
	aStream
		nextPutAll: (aSpaceTallyItem analyzedClassName padRightTo: 45);
		nextPutAll: (aSpaceTallyItem codeSize printString padLeftTo: 12);
		nextPutAll: (aSpaceTallyItem instanceCount printString padLeftTo: 12);
		nextPutAll: (aSpaceTallyItem spaceForInstances printString padLeftTo: 14).
	aStream
		nextPutAll: ((percent printShowingDecimalPlaces: 2) padLeftTo: 12);
		nextPutAll: (((aSpaceTallyItem spaceForInstances / (aSpaceTallyItem instanceCount max: 1)) asFloat printShowingDecimalPlaces: 2) padLeftTo: 20);
		cr
]

{ #category : 'fileout' }
SpaceTally >> privateAllObjects [
	Smalltalk garbageCollect.
	^ SystemNavigation default allObjects groupedBy: #class
]

{ #category : 'accessing' }
SpaceTally >> results [

	^ results
]

{ #category : 'fileout' }
SpaceTally >> saveTo: aFileName [
	"| st |
	st := SpaceTally new.
	st spaceTally: (Array with: TextMorph with: Point).
	st saveTo: 'spaceTally2'"
	| file |
	file := aFileName asFileReference.
	file exists ifTrue: [ file delete ].

	file writeStreamDo: [ :s|
		results items do: [:each | s nextPutAll: each analyzedClassName asString ;
						nextPutAll: ' '; nextPutAll: each codeSize printString;
						nextPutAll: ' '; nextPutAll: each instanceCount printString;
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; cr]]
]

{ #category : 'utilities' }
SpaceTally >> spaceForInstancesOf: aClass [
	"Answer a pair of the number of bytes consumed by all instances of the
	 given class, including their object headers, and the number of instances."

	| instances total |
	instances := aClass allInstances.
	instances isEmpty ifTrue: [^#(0 0)].
	total := 0.
	aClass isVariable
		ifTrue:
			[instances do:
				[:i| total := total + (aClass byteSizeOfInstanceOfSize: i basicSize)]]
		ifFalse:
			[total := instances size * aClass byteSizeOfInstance].
	^{ total. instances size }
]

{ #category : 'class analysis' }
SpaceTally >> spaceTally: classes [
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed 	by the code and instances of each class in the system. Note that code sizes do not currently 	report memory consumed by class variables. "

	"
	((SpaceTally new spaceTally: (Array with: TextMorph with: Point)) items asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) asArray
	"

	self preAllocateResultsFor: classes.
	Smalltalk garbageCollect.
	self computeSpaceUsage.
	^ results
]

{ #category : 'class analysis' }
SpaceTally >> systemWideSpaceTally [
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed 	by the code and instances of each class in the system. Note that code sizes do not currently 	report memory consumed by class variables. "

	"(SpaceTally new systemWideSpaceTally asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) asArray"

	self
		computeSpaceUsageResultsForClassesInstancesAssociations: self privateAllObjects.
	^ self results items
]
