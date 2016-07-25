accessing
nextPutAll: aCollection 
 	"Optimized access to get around Text at:Put: overhead"
 	| n |
 	n := aCollection size.
      position + n > writeLimit
        ifTrue:
         [self growTo: position + n + 10].
 	collection 
 		replaceFrom: position+1
 		to: position + n
 		with: aCollection
 		startingAt: 1.
 	position := position + n.
	^ aCollection