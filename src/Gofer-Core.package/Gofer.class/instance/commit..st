operations
commit: aString
	"Commit the modified packages with the given commit message."

	^ self execute: GoferCommit do: [ :operation | operation message: aString ]