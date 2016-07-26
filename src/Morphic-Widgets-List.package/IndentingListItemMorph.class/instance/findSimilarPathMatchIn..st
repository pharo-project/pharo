private
findSimilarPathMatchIn: anArray 
	self
		withSiblingsDo: [:each | (each complexContents asString sameAs: anArray first)
				ifTrue: [^ each]].
	^ nil