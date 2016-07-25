protocol
classes

	classes ifNil: [
		classes := Dictionary new.
		self patch
			ifNil: [ #() ] 
			ifNotNil: [ :p |
				p operations 
					do: [ :each | | komitClass | 
						each definition className ifNotNil: [
							komitClass := (self classNamed: each definition className).
							each definition addOperation: each on: komitClass ] ] ].
	].
	^ classes values