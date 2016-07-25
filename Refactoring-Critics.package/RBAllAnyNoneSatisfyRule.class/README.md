Replace ad-hoc implementations (using explicit logic based on do:) of ==allSatisfy:==, ==anySatisfy:== and ==noneSatisfy:== by the adequate calls to ==allSatisfy:==, ==anySatisfy:== or ==noneSatisfy:==. 

For example 

[[[  
collection do: [ :each |
				...
				condition
					ifFalse: [ ^ false ] ]
]]]
is transformed into 

[[[  
collection allSatisfy: [ :each | condition ]
]]]