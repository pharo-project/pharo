simulation
simulateBullets: delta
	| position velocity |
	
	bullets do: [ :bullet |
		position := bullet first.
		velocity	:= bullet second.
		position := position + (velocity * delta).
		bullet at: 1 put: position.
	].