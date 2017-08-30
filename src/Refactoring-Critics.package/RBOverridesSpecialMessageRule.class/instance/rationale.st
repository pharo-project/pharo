accessing
rationale 

	^ 'Checks that a class does not override a message that is essential to the base system. For example, if you override the #class method from object, you are likely to crash your image.
In the class the messages we should not override are: ',  (', ' join: (self classShouldNotOverride) ),'.
In the class side the messages we should not override are: ',  (', ' join: (self metaclassShouldNotOverride) ),'.' 