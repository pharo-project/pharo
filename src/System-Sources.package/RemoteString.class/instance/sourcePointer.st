accessing
sourcePointer
	sourceFileNumber ifNil: [ ^ 0 ].
	^ SourceFiles sourcePointerFromFileIndex: sourceFileNumber andPosition: filePositionHi