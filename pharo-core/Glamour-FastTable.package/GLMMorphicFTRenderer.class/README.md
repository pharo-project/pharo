I am an abstract class that define the way to render a FastTable related presentation.

Description
--------------------

I manage a FastTable dataSource and a FastTable morph. I define how to build and render them. I try to manage the maximum of the rendering and I let my subclasses define some specialities. 

My subclasses are use by a FastTable related presentation as GLMFastListPresentation or GLMFastTreePresentation. 
I use a FTTableMorph to manage the rendering and a subclass of FTDataSource to manage the data. 

Public API and Key Messages
--------------------

- #render: aPresentation 		This method is the main method of the class It allow to render myself on the presentation.
		
My subclasses should manage: 

- #createDataSourceFrom: aPresentation 		This method have to return  a new dataSource that match the presentation.
		
- #specificTableMorphInitializiation and #specificDataSourceInitializiation 		are two methods  my subclasses can override to add functionalites to the FastTable.

Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	tableModel:		Is a DataSource for Glamour that will hold the data of the presentation.
	tableMorph:		Is a FastTableMorph use to display the presentation.
