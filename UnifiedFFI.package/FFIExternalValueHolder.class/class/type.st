private
type
	^ type ifNil: [ type := FFIExternalType resolveType: self typeDecl ]