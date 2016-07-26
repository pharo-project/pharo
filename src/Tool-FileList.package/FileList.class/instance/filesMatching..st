private
filesMatching: aRegex
	"Answer a list of directory entries which match the patternString.
	The patternString may consist of multiple patterns separated by ';'.
	Each pattern can include a '*' or '#' as wildcards - see String>>match:"

	| files |
	files :=  [self directory files] on:DirectoryDoesNotExist do:[#()].
	pattern ifNil: [ ^ files ].
	^ files select: [:entry | (aRegex search: entry basename)]