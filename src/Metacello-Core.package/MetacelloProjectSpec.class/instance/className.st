querying
className
    className ifNil: [ self name ifNotNil: [ self className: self constructClassName ] ].
    ^ className