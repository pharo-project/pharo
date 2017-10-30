I represent two args collator block which must return 1, -1 or 0.

Usually I am created directly from block using sorting messages:
	[:a :b | ] ascending
	[:a :b | ] descending.

Look at SortFunction comments for more details.

Internal Representation and Key Implementation Points.

    Instance Variables
	collatorBlock:		<BlockClosure>	This is the collation function that must return a -1, 0, or 1