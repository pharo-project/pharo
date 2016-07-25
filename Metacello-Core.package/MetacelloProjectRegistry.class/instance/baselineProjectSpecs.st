accessing
baselineProjectSpecs
    "MetacelloProjectRegistration baselineProjectSpecs"

    | projectSpecs |
    projectSpecs := OrderedCollection new.
    self baselineRegistry
        keysAndValuesDo: [ :className :registration | projectSpecs add: (self projectSpecForClassNamed: className ifAbsent: [ self error: 'not expected' ]) ].
    ^ projectSpecs asArray