converting
asSignedInteger 
	"Returns the first signed integer it can find or nil."

	| start stream |
	start := self findFirst: [:char | char isDigit].
	start isZero ifTrue: [^ nil].
	stream := self readStream position: start - 1.
	((stream position ~= 0) and: [stream peekBack = $-])
		ifTrue: [stream back].
	^ Integer readFrom: stream