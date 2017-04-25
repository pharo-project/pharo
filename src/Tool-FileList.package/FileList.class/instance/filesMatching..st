private
filesMatching: aPattern
	"Answer a list of directory entries which match the patternString.
	The patternString may consist of multiple patterns separated by ';'.
	Each pattern can include a '*' or '#' as wildcards - see String>>match:"

	| files aRegex |
	files :=  [self directory files] on:DirectoryDoesNotExist do:[#()].
	aPattern ifNil:[^files].

	aRegex := aPattern asRegex.
	^ files select: [:entry | (aRegex search: entry basename)]