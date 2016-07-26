private
logUpdate: aPackage with: aVersion
	self log
		cr; nextPutAll: '========== ', aVersion info name, ' =========='; cr;
		cr; nextPutAll: aVersion info message asString; cr;
		flush.

	aPackage hasWorkingCopy ifFalse: [^self].

	aPackage workingCopy ancestors do: [:each |
		(aVersion info hasAncestor: each)
			ifTrue: [(aVersion info allAncestorsOnPathTo: each)
				do: [:ver | self log cr; nextPutAll: '>>> ', ver name, ' <<<'; cr;
							nextPutAll: ver message; cr; flush]]]