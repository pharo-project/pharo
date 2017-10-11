as yet unclassified
errorMessagesArray

	"return objects for error messages - special objects array at index 52"
	
 	^ {nil "nil => generic error". 
		#'bad receiver'. #'bad argument'. #'bad index'.
		#'bad number of arguments'.
		#'inappropriate operation'.  #'unsupported operation'.
		#'no modification'. #'insufficient object memory'.
		#'insufficient C memory'. #'not found'. #'bad method'.
		#'internal error in named primitive machinery'.
		#'object may move'. #'resource limit exceeded'.
		#'object is pinned'. #'primitive write beyond end of object'.
		#'object moved'. #'object not pinned'. #'callback error'} asLiteralInObjectSpace: objectSpace