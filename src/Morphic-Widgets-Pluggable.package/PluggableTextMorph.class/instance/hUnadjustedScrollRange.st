private
hUnadjustedScrollRange
"Return the width of the widest item in the list"

	textMorph ifNil: [ ^0 ].
	textMorph isWrapped ifTrue:[ ^0 ].

	^super hUnadjustedScrollRange
