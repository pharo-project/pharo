drawing
renderScene

	self checkSession.

	scene ifNotNil: [ 
		surface drawDuring: [ :acanvas | 
			surface clear.
			acanvas pathTransform loadAffineTransform: transform.
			scene isBlock
				ifTrue: [ scene cull: acanvas cull: self ]
				ifFalse: [ scene renderOn: acanvas ] ] ]