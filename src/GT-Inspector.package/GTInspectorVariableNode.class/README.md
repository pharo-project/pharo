I model an attribute of an object in the GTInspector. I am used in the Raw presentation of an Object. My main responsibility is to associate a variable name of a host object with its value. 

Public API and Key Messages

- hostObject return the object (host) holding the  attribute that I represent
- label return a string label of the attribute, which may be an instance variable name or a dynamically generated name for dynamic attributes.
- value return the value  of the attribute
- key return an object used as unique key to identify an attribute, it may be an integer for indexed variables or a string for dynamic ones
 
Internal Representation and Key Implementation Points.

    Instance Variables
	hostObject:		<Object>