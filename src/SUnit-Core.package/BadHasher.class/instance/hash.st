comparing
hash
	"answer with a different hash some of the time"
	100 atRandom < 30
		ifTrue: [^ 1]. 
	^ 2