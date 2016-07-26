accessing
previousProcessorFrom: aCollection
	aCollection do: [ :each | 
		each asSpotterCandidateLink processor ifNotNil: [ ^ each processor ] ]. " we assume they are all identical "
	^ GTSpotterCandidatesListProcessor new
		title: 'Items';
		filter: GTFilterSubstring;
		yourself