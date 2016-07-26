actions
applyHighlights
	"Apply the relevant highlights to src and dst."

	self srcMorph highlights: (self joinMappings flatCollect: [:j | j src highlights]).
	self dstMorph highlights: (self joinMappings flatCollect: [:j | j dst highlights])