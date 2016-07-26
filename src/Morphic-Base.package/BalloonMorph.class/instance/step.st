stepping and presenter
step
	"Move with target."

	target ifNotNil: [self position: target position + offsetFromTarget].
