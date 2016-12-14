running
createBehaviorFromDefinition: aRFiDefinition

	[ ^ super createBehaviorFromDefinition: aRFiDefinition ]
			on: AssertionFailure do: [ :e |
				((e signalerContext method selector == #assert:)
					and: [ e signalerContext sender method selector = #mapStackPages ]) ifFalse: [ e pass ]] 