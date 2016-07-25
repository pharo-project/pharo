utilities
changesIn: aPackage from: oldInfo to: newInfo on: aStream
	| printBlock newVersion |
	(newInfo = oldInfo)
		ifTrue: [^self].
	aStream cr; nextPutAll: '----------------- ', aPackage name, ' ------------------'; cr.

	newInfo 
		ifNil: [^aStream cr; nextPutAll: 'REMOVED'; cr].

	oldInfo 
		ifNil: [^aStream cr; nextPutAll: 'ADDED'; cr].

	"get actual version for full ancestry"
	newVersion := MCRepositoryGroup default versionWithInfo: newInfo.

	printBlock := [:ver | 
		aStream cr; 
			nextPutAll: (ver name copyAfterLast: $-);
			nextPutAll:  ' (', (ver date printFormat: #(1 2 0 $. 1 1 2)), ', '.
		ver time print24: true showSeconds: false on: aStream.
		aStream nextPutAll: ')'; cr; 
			nextPutAll: ver message; cr].

	(newVersion info hasAncestor: oldInfo)
		ifTrue: [(newVersion info allAncestorsOnPathTo: oldInfo)
			reverseDo: printBlock].
	newVersion info in: printBlock.
	aStream flush