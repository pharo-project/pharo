actions
acceptTextMorphs
	"Accept any text morphs except for those that have no edits."

	self allMorphs do: [:p |
		((p respondsTo: #accept) and: [
			(p respondsTo: #hasUnacceptedEdits) and: [
				p hasUnacceptedEdits]]) ifTrue: [p accept]]