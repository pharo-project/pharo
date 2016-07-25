accessing
registry
    Registry ifNil: [ Registry := MetacelloProjectRegistry new ].
    ^ Registry