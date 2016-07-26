*GT-Spotter
spotterPreviewIn: aComposite inContext: aSpotter

	| pragmas |
	pragmas := Pragma 
				allNamed: #spotterPreview: 
				from: self class 
				to: Object
				sortedUsing: [: x :y | (x argumentAt: 1) < (y argumentAt: 1)].
	"We iterate through all pragmas and execute the corresponding methods
	to ensure that all possible previews are taken into account.
	Even though at this moment we only display one preview, we still have to go through
	all pragmas because it can happen that the method has a guarding if and thus, the actual
	preview would not be triggered."
	pragmas do: [ :eachPragma | 
		(eachPragma selector findTokens: $:) size = 1 
			ifTrue: [ self perform: eachPragma selector with: aComposite ]. 
		(eachPragma selector findTokens: $:) size = 2
			ifTrue: [ self perform: eachPragma selector with: aComposite with: aSpotter ].
		aComposite hasSubbricks ifTrue: [ ^ self ]]