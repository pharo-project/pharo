Extension { #name : #ScaledDecimal }

{ #category : #'*NewMethodFinder' }
ScaledDecimal class >> approvedSelectors [

	^ #(scale setNumerator:denominator:scale: coerce:)
]
