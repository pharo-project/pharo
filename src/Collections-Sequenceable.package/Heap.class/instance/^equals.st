comparing
= anObject
	"Heap are considered equals only if they have same sort order and same elements."

	self == anObject ifTrue: [^true].
	anObject isHeap ifFalse: [^false].
	self sortBlock = anObject sortBlock ifFalse: [^false].
	self size = anObject size ifFalse: [^false].
	^(self asArray sort: sortBlock) = (anObject asArray sort: sortBlock)