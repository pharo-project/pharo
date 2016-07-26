private
privateRootClasses
	| classes |
	classes := OrderedCollection new.
	Class rootsOfTheWorld do: [ :each | 
		| class |
		class := self classFor: each.
		(class notNil and: [ class superclass isNil ]) 
			ifTrue: [ classes add: class ] ].
	^ classes