private
addItemShowing: aJob
	
	lock critical: [ | item items |
		item := JobProgressMorph job: aJob.
		items := self bars size.
		items < 10 ifTrue: [ 
			self addMorphBack: item.
			self resize ].
		^ item ].