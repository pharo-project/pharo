private
newViewOn: aText 
	| layoutView |
	layoutView :=  TxAthensLayoutView on: aText extent: self defaultLayoutViewExtent.
	layoutView layoutStrategy: self defaultStrategy.
	^ layoutView