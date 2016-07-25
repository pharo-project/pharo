*Spec-MorphicAdapters
pendingText
	"<api: #inspect>"
	"Return the current pending text"

	^ self 
		detectMorphicAdapterDo: [ :w | w pendingText ]
		ifNone: [ self getText ]