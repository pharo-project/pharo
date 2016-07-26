testing
hasMethodForSymbolicVersion: versionSymbol

	^(self constructor extractSymbolicVersionPragmas at: versionSymbol ifAbsent: [ ^ false ]) size > 0
