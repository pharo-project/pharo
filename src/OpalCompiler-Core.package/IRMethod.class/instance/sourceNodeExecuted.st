mapping
sourceNodeExecuted 
	"It should not be used directly. But it is needed for fallback call from RBMethodNode>>sourceNodeExecutedForPC:. 
	Problem that IRMethod can returns itself from  instructionForPC:.
	So it should only return sourceNode without any analyzis"
	^sourceNode