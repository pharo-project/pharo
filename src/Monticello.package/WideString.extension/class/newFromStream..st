*Monticello
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |

	s next = 16r80 ifTrue:
		["A compressed format.  Could copy what BitMap does, or use a 
		special sound compression format.  Callers normally compress their own way."
		^ self error: 'not implemented'].
	s skip: -1.
	len := s nextInt32.
	^ s nextWordsInto: (self basicNew: len)