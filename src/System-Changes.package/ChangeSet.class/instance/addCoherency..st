private
addCoherency: className
	"SqR! 19980923: If I recreate the class then don't remove it"

	(self changeRecorderFor: className)
		checkCoherence.
"
	classRemoves remove: className ifAbsent: [].
	(classChanges includesKey: className) ifTrue:
		[(classChanges at: className) remove: #remove ifAbsent: []]
"