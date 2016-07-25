geometry
basicMinExtent
	"Answer the unadjusted min extent."

	|minTabExtent controlExtent controlsExtent|
	controlsExtent := 0@0.
	self controls do: [:control |
		controlExtent := control minExtent.
		controlsExtent := controlsExtent x + controlExtent x @ (controlsExtent y max: controlExtent y)].
	self tabs ifEmpty: [^controlsExtent].
	minTabExtent := 0@0.
	self tabs do: [:tab | minTabExtent := minTabExtent max: tab minExtent].
	^minTabExtent x + controlsExtent x @ (minTabExtent y max: controlsExtent y)