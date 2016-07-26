removing
remove: oldObject ifAbsent: exceptionBlock 
	"Refer to the comment in Collection|remove:ifAbsent:."

	| count |
	count := contents at: oldObject ifAbsent: [^ exceptionBlock value].
	count = 1
		ifTrue: [contents removeKey: oldObject]
		ifFalse: [contents at: oldObject put: count - 1].
	^ oldObject