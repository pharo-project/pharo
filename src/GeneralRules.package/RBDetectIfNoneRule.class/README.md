Replaces detect:ifNone: and contains: when used with the semantics of anySatisfy: by anySatisfy:.

For example, 

[[[ 
collection detect: [:each | .... ] ifNone: [nil]) notNil	
]]]
is transformed into 

[[[ 
collection anySatisfy: [ :each | .... ])
]]]