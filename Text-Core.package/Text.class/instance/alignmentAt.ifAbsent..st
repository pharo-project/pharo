emphasis
alignmentAt: characterIndex ifAbsent: aBlock
	| attributes emph |
	self size = 0 ifTrue: [^aBlock value].
	emph := nil.
	attributes := runs at: characterIndex.
	attributes do:[:att | (att isKindOf: TextAlignment) ifTrue: [emph := att]].
	^ emph ifNil: aBlock ifNotNil: [emph alignment]