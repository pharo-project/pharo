character classification
isLetter: char

	| value leading |

	leading := char leadingChar.
	value := char charCode.

	leading = 0 ifTrue: [^ super isLetter: char].

	value := value // 94 + 1.
	^ 1 <= value and: [value < 84].
