accessing
sliderScrollForward

	vertical ifTrue: [ 
		viewContainer viewMorph scrollBy: 0@viewContainer viewMorph height
		]
	ifFalse: [ 
		viewContainer viewMorph scrollBy: viewContainer viewMorph width@0
		
		 ]