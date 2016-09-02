It is container of all mirror primitives implemented in VM.
Mirror primitives allow to call some primitive on object without sending message to it. It is achived by using receiver as first argument of primitive.

Mirror primitives  violates the principle that each object has sovereign control own state (for example  over the storing of values into its instance variables). But it is essential for the	 debugger. 
	
For details see  Object documentation whatIsAPrimitive.