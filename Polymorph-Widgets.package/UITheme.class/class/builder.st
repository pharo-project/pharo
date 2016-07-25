accessing
builder
	"Answer a morph that has the TEasilyThemed trait."

	^Builder ifNil: [Builder := self newBuilder. Builder]