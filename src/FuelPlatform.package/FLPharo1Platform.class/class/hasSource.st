private-extensions-compiledmethodtrailer
hasSource
	<flExtensionOf: #CompiledMethodTrailer isSource: true>
	^ 'hasSource
	^ kind == #EmbeddedSourceQCompress or: [ kind == #EmbeddedSourceZip ]'