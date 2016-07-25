public
stopDefault
	"Stop the default instance, if any, and clear it.
	Return the stopped instance, if any.
	Delegate to the default server class."

	^ self defaultServerClass stopDefault 