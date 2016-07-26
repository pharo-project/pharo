This smell arises when a large class is found (with 40 or more methods). Large classes are indications that it has too much responsibility. Try to break it down, and reduce the size to something manageable. The defined number of methods can be edit in RBExcessiveMethodsRule>>methodsCount.

A good design assigns one responsibility to a class. Ask yourself, "what is the key responsibility of this class?" Using the strategy design pattern may be a solution to structure and delegate some behavior. 

An indication that a class may have too many responsibilities is when different groups of methods access a subpart of the instance variables. In a large system, having some large classes is often inevitable; but when there are more than a couple of large classes, you should really reconsider your design. 
	
The defined number of methods can be edited in #methodsCount.