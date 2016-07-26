private-extensions-compiledmethod
codeForNoSource
	<flExtensionOf: #CompiledMethod>
	"this is the marker we use for method that have no source"
	^ String streamContents: [:str |
		str 
			nextPutAll: self selector asMethodPreamble;
			cr;tab;
			nextPutAll: 'self methodHasNoSourceCode' ]