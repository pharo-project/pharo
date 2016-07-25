resolving
resolve
	"Ok, consider that we will be resolved only once. We could use a become: to stop being a resolved version instead."

	| v |
	version ifNil: [ ^ super version ].
	v := version.
	version := nil.
	^ v