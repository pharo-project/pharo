initialize-release
initialize
	super initialize.
	transmissionStrategy := GLMNoStrategy new.
	condition := true