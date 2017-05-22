private
detachRemote: aRemote
	(self pushRemote isSame: aRemote) ifTrue: [ self pushRemote: nil ].
	(self pullRemote isSame: aRemote) ifTrue: [ self pullRemote: nil ]