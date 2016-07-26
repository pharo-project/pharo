i am a base class for path conversion. 
i take a path as input and producing a path commands as output.

My default implementation just passing all path commands without changes to destination without any conversion

Example of use:

 
converter := AthensXYZConverter dest: (AthensPolygonPath new).
convertedPath := converter convert: sourcePath.

You can chain multiple converters:

basicConverter := AthensZYXConv dest: AthensPolygonPath new. 
compoundConverter := AthensABCConverter dest: basicConverter.

compoundConverter convert: somePath

in this case, an initial conversion is performed by instance of AthensABCConverter, and then conversion results are passed down to AthensZYXConv,
and then finally to instance of AthensPolygonPath.
