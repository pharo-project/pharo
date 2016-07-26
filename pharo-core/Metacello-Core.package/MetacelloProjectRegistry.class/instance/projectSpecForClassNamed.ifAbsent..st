querying
projectSpecForClassNamed: aClassName ifAbsent: absentBlock
    ^ (self configurationRegistry
        at: aClassName
        ifAbsent: [ ^ (self baselineRegistry at: aClassName ifAbsent: [^absentBlock value]) baselineProjectSpec ])
        configurationProjectSpec