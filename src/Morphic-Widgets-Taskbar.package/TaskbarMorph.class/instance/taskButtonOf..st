taskbar
taskButtonOf: aMorph
	"Answer the task button of the given morph or nil if none."

	^self submorphs detect: [:t | t model = aMorph] ifNone: []