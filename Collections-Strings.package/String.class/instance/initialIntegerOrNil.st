converting
initialIntegerOrNil
	"Answer the integer represented by the leading digits of the receiver, or nil if the receiver does not begin with a digit"
	"
	'234Whoopie' initialIntegerOrNil
	'wimpy' initialIntegerOrNil
	'234' initialIntegerOrNil
	'2N' initialIntegerOrNil
	'2' initialIntegerOrNil
	'  89Ten ' initialIntegerOrNil
	'78 92' initialIntegerOrNil
	"

	| firstNonDigit |
	(self size = 0 or: [ self first isDigit not ]) 
		ifTrue: [ ^ nil ].
	firstNonDigit := (self findFirst: [ :m | m isDigit not ]).
	firstNonDigit = 0 ifTrue: [ firstNonDigit := self size + 1 ].
	^ (self copyFrom: 1  to: (firstNonDigit - 1)) asNumber