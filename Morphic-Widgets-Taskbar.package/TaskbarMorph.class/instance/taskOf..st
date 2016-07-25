taskbar
taskOf: aMorph
	"Answer the task of the given morph or nil if none."

	^self orderedTasks detect: [:t | t morph = aMorph] ifNone: []