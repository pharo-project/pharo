navigation
nextPage
	| last |
	nextPageAllowed
		ifTrue: [[nextPageAllowed := false.
			pageSizeEditor
				ifNotNil: [pageSizeEditor acceptTextInModel].
			self changed: #pageSize.
			treeMorph scroller hasSubmorphs
				ifTrue: [last := treeMorph scroller lastSubmorph].
			self
				nextPage: (self lastIndex + pageSize min: self nodeList size).
			last
				ifNotNil: [| b1 b2 |
					b2 := (treeMorph bounds: treeMorph innerBounds in: self world) bottom - treeMorph vExtraScrollRange.
					b1 := last boundsInWorld bottom.
					treeMorph scrollBy: 0 @ (b2 - b1)].
			self buildPanel]
				ensure: [nextPageAllowed := true]] 