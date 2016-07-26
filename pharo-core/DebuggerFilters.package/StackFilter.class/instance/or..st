operators
or: aFilter
	  ^ BooleanFilter withFilters: (OrderedCollection with: self with: aFilter) operator: #or: