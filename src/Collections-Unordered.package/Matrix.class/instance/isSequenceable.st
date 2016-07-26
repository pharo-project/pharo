testing
isSequenceable
	"LIE so that arithmetic on matrices will work.
	 What matters for arithmetic is not that there should be random indexing
	 but that the structure should be stable and independent of the values of
	 the elements.  #isSequenceable is simply the wrong question to ask."
	^ true