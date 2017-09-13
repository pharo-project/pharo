This smell arises when a method is implemented but never sent. If a method is not sent, it can be removed. This rule pays attention not to identify as unsent methods, methods with pragmas and test methods since they are likely to be sent through reflection.
	Now if your code is used and extended by others better use a deprecation mechanism. To define a deprecate method follow the pattern: 
	
	foo
		self deprecated: ''Use bar instead ''. 
		^ self bar
		 