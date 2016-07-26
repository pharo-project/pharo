A MCModel2MTModelVisitor visits a Metacello model to transform it to a MT Model .

Instance variables:
	- project : the root element (an MTProject) of the target model
	
	
MCModel2MTModelVisitor new 
	visitConfiguration: ConfigurationOfVersionner  
	withVersion: (ConfigurationOfVersionner project version: #development)