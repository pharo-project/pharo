private
findWherePresentationIs: aPresentation in: aMorph
	"Explores aMorph hierarchy of sub morphs to find a morph whose model is exactly aModel"
	^ self findSatisfying: 
				[:morph | morph model notNil 
					and: [(morph model respondsTo: #glamourPresentation) 
					and: [morph model glamourPresentation == aPresentation]] ]
			in: aMorph  