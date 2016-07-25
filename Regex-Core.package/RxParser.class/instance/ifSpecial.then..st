private
ifSpecial: aCharacter then: aBlock
	"If the character is such that it defines a special node when follows a $\,
	then create that node and evaluate aBlock with the node as the parameter.
	Otherwise just return."

	| classAndSelector |
	classAndSelector := BackslashSpecials at: aCharacter ifAbsent: [^self].
	^aBlock value: (classAndSelector key new perform: classAndSelector value)