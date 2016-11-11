baseline
baseline: spec
	<baseline>

	spec for: #common do: [
		spec preLoadDoIt: #'preload:package:'.
		spec postLoadDoIt: #'postload:package:'.
		
		spec 
			package: 'Graphics-Display Objects';
			package: 'Graphics-Primitives';
			package: 'Graphics-Transformations'.
		spec 
			group: 'default' with: #('Graphics-Display Objects' 'Graphics-Primitives' 'Graphics-Transformations'). ].		
