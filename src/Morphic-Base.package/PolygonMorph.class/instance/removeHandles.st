menu
removeHandles
	handles ifNotNil: [
		handles do: [:h | h delete].
		handles := nil].