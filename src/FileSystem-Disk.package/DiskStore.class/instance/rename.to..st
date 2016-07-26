public
rename: sourcePath to: destinationPath

	| sourcePathString encodedSourcePathString targetPathString encodedTargetPathString result |
	sourcePathString := self stringFromPath: sourcePath.
	encodedSourcePathString := Primitives encode: sourcePathString.
	targetPathString := self stringFromPath: destinationPath.
	encodedTargetPathString := Primitives encode: targetPathString.
	^ Primitives rename: encodedSourcePathString to: encodedTargetPathString.