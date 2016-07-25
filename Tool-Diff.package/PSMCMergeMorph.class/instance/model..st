initialization
model: aMerger
	"Set the model and update the window."

	|grouped sorted|
	super model: aMerger.
	grouped := aMerger operations
		groupedBy: [:o | o targetClassName ifNil: [o shortSummary]]
		having: [:g | true].
	sorted := OrderedCollection new.
	grouped keys asSortedCollection do: [:k |
		sorted addAll: ((grouped at: k) asSortedCollection: [:a :b | a shortSummary <= b shortSummary])].		
	self patchMorph model: sorted