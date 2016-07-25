accessing
commonPrefix
	"Longest match among the typed text and all menu entries"

	| seed matchLengths prefixLength |
	seed := self entries first contents asString.
	matchLengths := entries allButFirst
		collect: [ :entry | seed charactersExactlyMatching: entry contents asString ]
		thenSelect: [ :e | e >= narrowString size ].
	prefixLength := matchLengths isEmpty
		ifTrue: [ 0 ]
		ifFalse: [ matchLengths min ].
	^ seed first: prefixLength.