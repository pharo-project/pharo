utilities
allSystemPragmas
	^ (Array
		streamContents: [:stream | SystemNavigation new
				allBehaviorsDo: [:behavior | Pragma
						withPragmasIn: behavior
						do: [:pragma | stream nextPut: pragma]]]) 