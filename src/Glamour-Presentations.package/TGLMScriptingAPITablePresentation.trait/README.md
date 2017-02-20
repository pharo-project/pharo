I provide an API for adding table columns having various  properties. I should be used by glamour presentations representing tables. I also provide the hook methods called be the Glamour renderer (see the *callbacks* protocol).

Public API and Key Messages
--------------------
Main methods:

- #column: evaluated:		        to add a configured column; there are many other methods that adds columns and configure different properties
		
- #addColumn: 		                  to directly add a column object
		
- #hideHeader                                  to hide the header of all columns