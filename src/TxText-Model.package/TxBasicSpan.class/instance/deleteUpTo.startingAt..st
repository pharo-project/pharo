operation
deleteUpTo: end startingAt: start
	"start and end pointing to receiver, nothing to remove"
	self assert: start span == self.
	
	end span == self ifTrue: [
		start spanPosition > 0 ifTrue: [ "nothing to delete" ^ end ].
		end spanPosition = 1 ifTrue: [ self delete. ^ next startPosition ].
		^ end
	].
	
	start spanPosition = 0 ifTrue: [
		self delete.
	].
	^ next deleteUpTo: end startingAt: next startPosition.
