I represent an occurrence of a pragma in a compiled method.  A pragma is a literal message pattern that occurs between angle brackets at the start of a method after any temporaries.  A common example is the primitive pragma:
	<primitive: 123 errorCode: 'errorCode'>
but one can add one's own and use them as metadata attached to a method.  Because pragmas are messages one can browse senders and implementors and perform them.  One can query a method for its pragmas by sendng it the pragmas message, which answers an Array of instances of me, one for each pragma in the method.

I can provide information about the defining class, method, its selector, as well as the information about the pragma keyword and its arguments. See the two 'accessing' protocols for details. 'accessing-method' provides information about the method the pragma is found in, while 'accessing-pragma' is about the pragma itself.

Instances are retrieved using one of the pragma search methods of the 'finding' protocol on the class side.

To browse all methods with pragmas in the system evaluate
	SystemNavigation new browseAllSelect: [:m| m pragmas notEmpty]
and to browse all nonprimitive methods with pragmas evaluate
	SystemNavigation new browseAllSelect: [:m| m primitive isZero and: [m pragmas notEmpty]]