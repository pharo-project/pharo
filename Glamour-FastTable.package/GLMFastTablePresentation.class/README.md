I'm a fast table presentation who introduces FTTableMorph into Glamour. 

Description
--------------------

I know how to render myself and I store some options for the Table.  I keep some options specific to the FastTable inside a Dictionary. 

I work with GLMMorphicFastTableRenderer to render the FastTable. I use TGLMFastTableFunctionsPresentation in order to manage some options.

Public API and Key Messages
--------------------

You can use the public API of my super class. You also use the public API of TGLMFastTableFunctionsPresentation (See his class comment).

For now I do not add any other api methods.

Example
--------------------

GLMWrapper new 
	show: [ :a | 
		a fastTable
			display: [ :x | 1 to: x ]; 
			column: [:x | 'Numbers from 1 to ', x asString] evaluated: #asString;
			column: 'Even' evaluated: [ :each | each even asString ];
			column: 'Odd' evaluated: [ :each | each odd asString ]  ];
	openOn: 1000.

Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	parameters:		This is a Dictionary use to store some options related to the FastTable.
