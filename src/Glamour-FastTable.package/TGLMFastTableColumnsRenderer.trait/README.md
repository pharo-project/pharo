I am a Trait that provides methods for creating rows in a Glamour presentation with columns. I exist mainly ro provides code reuse. 

Description 
--------------------

My main entry point is #cellColumn:row:. This is the method called by FTTableMorph on a data source for obtaining the rows to display.  I can configure rows to also be editable.


Internal Representation and Key Implementation Points.
--------------------

- #rowMorphElementsForFirstColumn:item:in:    I am a  hook method for configuring the first column (tables and tree need a different first column)
- #dataFromPresentationItem:                                     I am a hook methods for getting the actual data from an item of the FTTableMorph