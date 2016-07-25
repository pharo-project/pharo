analyzing
add: anObject traceWith: aAnalysis
	"Add an object to the cluster and trace references."

	objects addIfNotPresent: anObject ifPresentDo: [ ^ self ].

	self 
		referencesOf: anObject 
		do: [ :aChild || actual |
			actual := aChild fuelReplacement.
			self addReferenceFrom: anObject to: actual.
			aAnalysis trace: actual ]