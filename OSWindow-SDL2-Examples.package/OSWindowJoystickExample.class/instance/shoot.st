game controls
shoot
	| position velocity |
	position := characterPosition.
	velocity := 400 @ characterVelocity y.
	bullets add: { position . velocity }