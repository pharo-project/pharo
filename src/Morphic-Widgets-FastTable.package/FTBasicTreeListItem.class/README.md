FTBasicTreeListItem holds the state of a element in FTBasicTreeListDataSource.

Instance variables: 

|< expanded >|< Private  >|< aBoolean >[ 
	remembers if this it is currently expanded or not.
}.

|< depth >|< Private  >|< aNumber >[
	The depth of this item from the root.
 ].

< item > [  anyObject. 
	The raw item which is wrapped by this class.	
].

< children > [ aCollection.
	cache for holding currently expaned children
 ].

< dataSource > [ aFBasicTreeListDataSource
	reference to the datasource
 ].