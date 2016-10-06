private
sourceNodeInterval
	sourceNode ifNil: [ ^ 1 to: 0 ].
	^ sourceNode keywordsPositions first to: sourceNode stop