protected low level
contents 
"	Cannot directly return == stream contents == since Morphic assumes multiple calls will return 
	the same value within the one cycle.  WorldState>>runStepMethodsIn:  sends #step to 
	do == stepContents := stream contents ==.
"
	^ stepContents