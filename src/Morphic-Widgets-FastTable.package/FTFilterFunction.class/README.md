I define a filter function for a table. 
I respond to any alphanumeric element and I add a filter box to the  owner table. 

In general, my entry point is through #keyStroke:, because I intend to react to keyboard inputs in the owner table.

I save an initial data source if the user want to see some result already filter.

I use a semaphore in order to let a delay before I filter the table. With this the user is able to type more than 1 letter before I filter.

/!\ To use me the data source must implement the method #newDataSourceMatching: aRegex