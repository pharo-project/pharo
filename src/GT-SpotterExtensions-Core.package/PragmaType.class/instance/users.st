accessing
users
	^ (self keyword senders collect: [ :each | each compiledMethod ]) \ (self pragmas collect: [ :each | each method ])