tests - converting
testSubstrings
	| allByteCharacters substring separators |

	allByteCharacters := Character allByteCharacters asString.
	substring := allByteCharacters select: [:each | each isLetter].
	separators := allByteCharacters reject: [:each | each isLetter].
	{'' -> {}.
		separators -> {}.
		substring -> {substring}.
		(substring, separators) -> {substring}.
		(separators, substring) -> {substring}.
		(separators, substring, separators) -> {substring}.
		(substring, separators, substring) -> {substring. substring}.
		(substring, separators, substring, separators) -> {substring. substring}.
		(separators, substring, separators, substring) -> {substring. substring}}
		do: [:each | self assert: (each key substrings: separators) = each value].

	"test the unary version"
	substring := allByteCharacters reject: [:each | each isSeparator].
	separators := allByteCharacters select: [:each | each isSeparator].
	{'' -> {}.
		separators -> {}.
		substring -> {substring}.
		(substring, separators) -> {substring}.
		(separators, substring) -> {substring}.
		(separators, substring, separators) -> {substring}.
		(substring, separators, substring) -> {substring. substring}.
		(substring, separators, substring, separators) -> {substring. substring}.
		(separators, substring, separators, substring) -> {substring. substring}}
		do: [:each | self assert: each key substrings = each value].