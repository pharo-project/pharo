Extension { #name : #Magnitude }

{ #category : #'*NewMethodFinder' }
Magnitude class >> approvedSelectors [

	^ #(< <= > >= between:and: compareWith:ifLesser:ifEqual:ifGreater: max: min: min:max: threeWayCompareTo)
]
