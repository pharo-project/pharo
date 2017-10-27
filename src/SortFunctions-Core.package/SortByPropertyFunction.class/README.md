I implement collation of objects using their property. Property is defined by selector or one arg block which can be converted directly to my instances:
	#name ascending.
	#name descending.
	[:a | a name] ascending
	[:a | a name] descending
I provide extra setting to specify what to do with undefined properties which value is nil.
	#name ascending undefinedFirst
- will put all objects with nil in name into the begin of order
	#name ascending undefinedLast
- will put all objects with nil in name into the end of order

Look at SortFunction comments for more details.

Internal Representation and Key Implementation Points.

    Instance Variables
	propertyValuable:		<Symbol, BlockClosure>
	undefinedDirection:		<SmallInteger>	1 nil's at first, -1 for nil's at last