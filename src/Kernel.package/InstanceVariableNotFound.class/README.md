Description
--------------------------

I am an exception raised when we try to access to an instance variable by its name but the receiver does not know this instance variable.

Examples 
--------------------------

	InstanceVariableNotFound signalFor: 'test'.
	
	Object new instVarNamed: 'test'.
	
	[ Object new instVarNamed: 'test' ] on: InstanceVariableNotFound do: [ :ex | ex ].

Internal Representation and Key Implementation Points.
--------------------------

    Instance Variables
	instVarName:		<aString>	Name of the instance variable we tried to access.