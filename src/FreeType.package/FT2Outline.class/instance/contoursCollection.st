accessing
contoursCollection
	"returns a list of contours with tag => points list pairs"
	| allPoints result start end |
	allPoints := self pointCollection.
	
	result := OrderedCollection new.
	start := 1.
	"no normal iteration because contours size can be bigger than contourSize"
	1 to: contoursSize do: [ :i |
		end := (contours at: i) + 1. "c converion"
		result add: ((tags copyFrom: start to: end) ->  (allPoints copyFrom: start to: end)).
		start := end + 1.
	].

	^result

		

		
