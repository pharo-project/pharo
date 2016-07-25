execute
execute
	[ 
		[
			failed := false.
			operation value
		] on: Exception do: [ :ex |
			failed := true.
			ex freeze.
			[ ex debug ] fork.
		]
	] ensure: [ semaphore signal ]