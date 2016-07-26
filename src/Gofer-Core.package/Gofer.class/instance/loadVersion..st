operations
loadVersion: anMetacelloVersionIdentifierSymbol
	"Load the given version name"
	^ self execute: GoferMetacelloLoad do: [ :operation|
		operation version: anMetacelloVersionIdentifierSymbol ]