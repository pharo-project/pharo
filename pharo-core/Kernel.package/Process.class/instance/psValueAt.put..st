process specific
psValueAt: index put: value
	"Set a value for given index in process-specific storage"

	"NOTE: this method are PRIVATE. Do not use it directly, instead use ProcessSpecificVariable (or its subclasses) "

	env ifNil: [ env := WeakArray new: PSKeys size ].
	env size < PSKeys size ifTrue: [ env := env grownBy: PSKeys size - env size ].
	^ env at: index put: value.