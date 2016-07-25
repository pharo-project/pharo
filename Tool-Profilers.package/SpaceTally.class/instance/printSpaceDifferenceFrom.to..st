fileout
printSpaceDifferenceFrom: fileName1 to: fileName2
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		SpaceTally new printSpaceAnalysis: 0 on: 'STspace.text1' asFileReference writeStream.
			--- do something that uses space here ---
		SpaceTally new printSpaceAnalysis: 0 on: 'STspace.text2' asFileReference writeStream.
		SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'
"
	| f coll1 coll2 item |
	f := FileStream readOnlyFileNamed: fileName1.
	coll1 := OrderedCollection new.
	[f atEnd] whileFalse: [coll1 add: f nextLine].
	f close.
	f := FileStream readOnlyFileNamed: fileName2.
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