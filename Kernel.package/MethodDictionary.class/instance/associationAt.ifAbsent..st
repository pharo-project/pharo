accessing
associationAt: key ifAbsent: aBlock
      "Answer the association with the given key.
       If key is not found, return the result of evaluating aBlock."

       ^(array at: (self scanFor: key))
               ifNil: [ aBlock value ]
               ifNotNil: [ :value | key -> value ]