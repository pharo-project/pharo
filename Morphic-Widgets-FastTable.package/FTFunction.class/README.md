I am an abstract class. My subclasses are some functions for a FastTable as search or filter.

Description
-------------------------------------------------
I can be use in two way.  Implicitely I will not appear on the FastTable. Explicitely I can display a Widget on the FastTable.
I work with  a FTTableMorph. I cannot be use alone.

Public API and Key Messages
-------------------------------------------------

- #table: aTableMorph
	is my constructor
	
- #keyStroke: anEvent
	This is the method that will allow to use me implicitely. With this I will receive an event from the FastTable.
	
- #beExplicite
	This method will make my functionnality explicit. For example the FTFilterFunction will display a filter field.
	
Example (Should only be create by a FTTableMorph)
-------------------------------------------------

FTFunction table: (FTTableMorph with: (1 to: 200))


Internal Representation and Key Implementation Points.
-------------------------------------------------

    Instance Variables
	table 	I am a FTTableMorph that use this function.
