operators
and: aFilter
	  ^ BooleanFilter withFilters: (OrderedCollection with: self with: aFilter) operator: #and: