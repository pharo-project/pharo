api
childrenForObject: aMorph
	^ aMorph submorphs collect: [ :each |
		DynamicEyeElement
			host: aMorph
			value: each
		 ]