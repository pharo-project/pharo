icons behavior
runExamplarMethod: aMethod
	"Perform the method as an example method. Now since this is an examplar (an bare instance of the class) with in addition inspect it so that the user can get a cool inspector to play with it."
 
	(aMethod methodClass baseClass perform: aMethod selector) inspect