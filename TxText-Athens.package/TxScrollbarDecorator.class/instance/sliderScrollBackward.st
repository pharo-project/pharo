accessing
sliderScrollBackward

	vertical ifTrue: [ 
		viewContainer viewMorph scrollBy: 0@viewContainer viewMorph height negated
		]
	ifFalse: [ 
		viewContainer viewMorph scrollBy: viewContainer viewMorph width negated @0
		
		 ]