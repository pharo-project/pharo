visiting
visitMethodDefinition: definition
    (self methodDefinitions
        at: definition className
        ifAbsent: [ self methodDefinitions at: definition className put: OrderedCollection new ]) add: definition