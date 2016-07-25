running
basicCheck: aClass 
	| count |
	count := 0.
	((Set withAll: (aClass whichSelectorsReferTo: #addDependent:))
		addAll: (aClass whichSelectorsReferTo: #removeDependent:);
		yourself) do: 
		[ :sel | 
		(aClass compiledMethodAt: sel) messages do: 
			[ :each | 
			each = #addDependent: ifTrue: [ count := count + 1 ].
			each = #removeDependent: ifTrue: [ count := count - 1 ] ] ].
	^ count > 0