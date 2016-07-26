measuring
ascent
	| asc desc h |
	cachedAscent ifNotNil:[^cachedAscent].
	asc := self basicAscent.
	desc := self descent.
	h := self height.
	asc + desc < h ifFalse:[^cachedAscent := asc].
	"height is greater than asc+desc, adjust ascent to include the difference"
	^cachedAscent := h - desc 