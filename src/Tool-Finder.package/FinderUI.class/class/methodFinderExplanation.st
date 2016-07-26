explanations
methodFinderExplanation
	"The comment in the bottom pane"

	false ifTrue: [MethodFinder methodFor: #( (4 3) 7  (0 5) 5  (5 5) 10)].
		"to keep the method methodFor: from being removed from the system"

	^ 'Type a fragment of a selector in the search box and click <Search>.

Or, use an example to find a method in the system.  An example is made up of the following three items separated by a period: receiver. args. answer.    For example, type: 3. 4. 7. into the search box and click <Search>

Alternatively, in this bottom pane, use #methodFor: directly to find a method in the system.  Select this line of code and choose "print it".  

	MethodFinder methodFor: #( (4 3) 7  (0 5) 5  (5 5) 10).
This will discover (data1 + data2).

You supply inputs and answers and the system will find the method.  Each inner array is a list of inputs.  It contains the receiver and zero or more arguments.  For Booleans and any computed arguments, use brace notation.

	MethodFinder methodFor: { {1. 3}. true.  {20. 10}. false}.
This will discover the expressions (data1 < data2), (data2 > data1), and many others.

	MethodFinder methodFor: { {''29 Apr 1999'' asDate}. ''Thursday''.  
		{''30 Apr 1999'' asDate}. ''Friday'' }.
This will discover the expression (data1 weekday)

Receiver and arguments do not have to be in the right order.
See MethodFinder.verify for more examples.'