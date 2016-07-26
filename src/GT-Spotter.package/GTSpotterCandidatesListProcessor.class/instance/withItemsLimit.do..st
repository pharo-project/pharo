private
withItemsLimit: aLimit do: aBlock
	| previousCandidatesLimit |
	previousCandidatesLimit := candidatesLimit.
	^ [ candidatesLimit := aLimit. aBlock value ] 
		ensure: [ candidatesLimit := previousCandidatesLimit ]