I'm a model datasource to add columns support to a tree presentation.

Description 
--------------------

My superclass models a tree. I override  #cellColumn:row: to return a row that contains multiple columns.

I work with a GLMFastTreeTablePresentation and a GLMMorphicFastTreeWithColumnsRenderer.  I rely on TGLMFastTableColumnsRenderer for the logic that actually builds the row morphs. 

Internal Representation and Key Implementation Points.
--------------------

- #rowMorphElementsForFirstColumn:item:in:    I configure the first column with the propper indentation and buttons for expanding collapsing the node.
- #dataFromPresentationItem:                                     I return the data from the given item

    Instance Variables
	columnToSortBy		This is the column to use for sorting the displayed elements

