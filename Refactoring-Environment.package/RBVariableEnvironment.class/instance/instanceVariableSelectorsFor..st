private
instanceVariableSelectorsFor: aClass 
	| selectors |
	selectors := Set new.
	#(#instanceVariables #instanceVariableReaders #instanceVariableWriters) 
		with: #(#whichSelectorsAccess: #whichSelectorsRead: #whichSelectorsAssign:)
		do: [:var :sel | 
			| instVars |
			instVars := Set new.
			(self perform: var) keysDo: 
					[:each | 
					| cls |
					cls := self classForName: each.
					(cls notNil and: [aClass includesBehavior: cls]) 
						ifTrue: [instVars addAll: ((self perform: var) at: each)]].
			instVars do: [:each | selectors addAll: (aClass perform: sel with: each)]].
	^selectors