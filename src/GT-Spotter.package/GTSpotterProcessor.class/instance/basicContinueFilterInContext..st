private
basicContinueFilterInContext: aSpotterContext

	" We DO NOT prepare the context since we could no longer continue producing filtered candidates. "
	(self shouldFilterInContext: aSpotterContext) ifTrue: [
		" The continue-filter only puts more items on the stream but doesn't do any filtering at all. Therefore the original collection (=result/candidates) and its size remains the same and can be reused after the continue-filter has added more items. So we do not ressign the result to #allFilteredCandidates as it is done for the regular filter-loop. #allFilteredCandidates must never be modified more than once because the next #diveIntoCategory would render an invalid set of items !!! "
		self doContinueFilterInContext: aSpotterContext ].
	aSpotterContext stream 
		performSymbol: #onAmountChanged:in:
		withArguments: { self allFilteredCandidates size . self }