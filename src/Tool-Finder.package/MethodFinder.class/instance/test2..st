tests
test2: anArray
	"look for bad association"

	anArray do: [:sub |
		sub class == Association ifTrue: [
			(#('true' '$a' '2' 'false') includes: sub value printString) ifFalse: [
				self error: 'bad assn'].
			(#('3' '5.6' 'x' '''abcd''') includes: sub key printString) ifFalse: [
				self error: 'bad assn'].
		].
		sub class == Array ifTrue: [
			sub do: [:element | 
				element isString ifTrue: [element first asciiValue < 32 ifTrue: [
						self error: 'store into string in data']].
				element class == Association ifTrue: [
					element value class == Association ifTrue: [
						self error: 'bad assn']]]].
		sub class == Date ifTrue: [sub year isInteger ifFalse: [
				self error: 'stored into input date!!']].
		sub class == Dictionary ifTrue: [
				sub size > 0 ifTrue: [
					self error: 'store into dictionary']].
		sub class == OrderedCollection ifTrue: [
				sub size > 4 ifTrue: [
					self error: 'store into OC']].
		].