paths
createPath: aPathCreatingBlock
	"Create a path from provided path builder instance"
	
	| builder |
	builder := AthensSimplePathBuilder new.
	aPathCreatingBlock value: builder.

	^ AthensBalloonPath new convertFromBuilder: builder.
	
	