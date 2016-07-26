converting
reversed
 
 	"Answer a copy of the receiver with element order reversed."
 
 	^ self class string: string reversed runs: runs reversed.
 
   "  It is assumed that  self size = runs size  holds. "