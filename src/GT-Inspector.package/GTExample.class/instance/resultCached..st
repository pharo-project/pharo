private
resultCached: aResult
	self ignoreCache ifFalse: [ result := aResult ].
	GTExampleNautilus methodUpdated: self method with: aResult.
	^ aResult