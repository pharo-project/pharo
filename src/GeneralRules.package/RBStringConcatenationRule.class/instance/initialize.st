initialization
initialize
	super initialize.
	self  matchesAny: #(
		'``@collection do: ``@argument' 
		'``@collection do: ``@argument separatedBy: ``@argument2' 
		'``@start to: ``@stop do: ``@argument' 
		'``@collection detect: ``@argument' 
		'``@collection detect: ``@argument ifNone: ``@argument2' 
		'``@collection select: ``@argument'
		'``@collection reject: ``@argument' 
		'``@collection inject: ``@value into: ``@argument'
		'``@collection anySatisfy: ``@argument'
		'``@collection allSatisfy: ``@argument'
		'``@collection noneSatisfy: ``@argument' )