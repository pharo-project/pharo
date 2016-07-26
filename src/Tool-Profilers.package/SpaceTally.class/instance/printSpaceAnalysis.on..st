fileout
printSpaceAnalysis: threshold on: aStream
	"SpaceTally new printSpaceAnalysis: 1 on: (FileStream forceNewFileNamed: 'STspace.text')"

	"sd-This method should be rewrote to be more coherent within the rest of the class 
	ie using preAllocate and spaceForInstanceOf:"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| totalCodeSpace totalInstCount totalInstSpace n totalPercent |
	Smalltalk garbageCollect.
	totalCodeSpace := totalInstCount := totalInstSpace := n := 0.
	results := OrderedCollection new: Smalltalk classNames size.
	'Taking statistics...'
		displayProgressFrom: 0
		to: Smalltalk classNames size
		during: [ :bar | 
			Smalltalk globals allClasses
				do: [ :cl | 
					| codeSpace  instSpace   instCount |
					bar current: (n := n + 1).
					codeSpace := cl spaceUsed.
					Smalltalk garbageCollectMost.
					instCount := cl instanceCount.
					instSpace := cl instancesSizeInMemory.
					totalCodeSpace := totalCodeSpace + codeSpace.
					totalInstCount := totalInstCount + instCount.
					totalInstSpace := totalInstSpace + instSpace.
					results
						add:
							(SpaceTallyItem
								analyzedClassName: cl name
								codeSize: codeSpace
								instanceCount: instCount
								spaceForInstances: instSpace) ] ].
	totalPercent := 0.0.
	self printHeader: aStream.
	threshold > 0
		ifTrue: [ 
			"If inst count threshold > 0, then sort by space"
			results := (results
				select: [ :s | s instanceCount >= threshold or: [ s spaceForInstances > (totalInstSpace // 500) ] ])
				asSortedCollection: [ :s :s2 | s spaceForInstances > s2 spaceForInstances ] ].
	results
		do: [ :s | 
			| percent |
			percent := s spaceForInstances * 100.0 / totalInstSpace roundTo: 0.1.
			totalPercent := totalPercent + percent.
			self printSpaceTallyItem: s percent: percent on: aStream ].
	aStream
		cr;
		nextPutAll: ('Total' padRightTo: 45);
		nextPutAll: (totalCodeSpace printString padLeftTo: 12);
		nextPutAll: (totalInstCount printString padLeftTo: 12);
		nextPutAll: (totalInstSpace printString padLeftTo: 14);
		nextPutAll: ((totalPercent roundTo: 0.1) printString padLeftTo: 12)