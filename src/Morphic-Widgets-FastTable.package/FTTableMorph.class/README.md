I'm an implementation of a table, in a not-naive way. 

I assume I can have many rows, then I do not try to show all of them at once. Instead, I keep a datasource and I demand rows when needed (datasource implements a flyweight to fill the visible rows).

I should not be subclasse. An extension of FastTable should happen on a data source an not here. Extend me ONLY if it is impossible to do what you want on the data source.

Examples: 
-------------
FTTableMorph new
	extent: 200@400;
	dataSource: (FTSimpleDataSource elements: (1 to: 10000) );
	openInWindow.
	
You can check better examples in FTExamples

A FastTable have the possibility to be searchable, this is activate by default. 
You can disable this with the method #disableSearch.
But you also have the possibility to make your FastTable filterable with the method #enableFilter. But search and filter cannot be use in the same time.