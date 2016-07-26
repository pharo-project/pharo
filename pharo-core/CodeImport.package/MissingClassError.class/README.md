I'm a specialized error that is invoked when trying to load a method for a non existing class. 

By default this exception is not resumable but it can be set and used for example as follow:


	[ FileStream fileIn: aFile ]
		on: MissingClassError
		do: [ :exception | 
				exception defineClass. 
				exception asResumable.
				exception resume. ]
			
			
So we give the possibility to compile and resume compution.
The method defineClass defines a simple class inheriting from Object (or from the class specified using #superclassName:)
The idea is that if later the effective class is loaded its definition will override  this one. 