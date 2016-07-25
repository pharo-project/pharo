accessing
providedLocatedMethod
	| locatedMethod aLocatedMethod refOrigin |
	locatedMethod := nil.
	self locatedMethods ifEmpty: [ ^ nil].

	self locatedMethods size > 1
		ifTrue: [ 	aLocatedMethod := self locatedMethods anyOne.
						refOrigin := (aLocatedMethod methodClass >> aLocatedMethod selector) origin.
						(self locatedMethods 
							allSatisfy: [:each | each origin == refOrigin])
						ifTrue: [^ aLocatedMethod].  ].
	
	self locatedMethods do: [:each |
		each isProvided ifTrue: [
			locatedMethod isNil ifFalse: [^nil].
			locatedMethod := each]].
	^locatedMethod