serialize/materialize
materializeInstancesStepWith: aDecoder

	1 to: objects size do: [ :index |
		objects 
			at: index 
			put: (self materializeInstanceWith: aDecoder) ].