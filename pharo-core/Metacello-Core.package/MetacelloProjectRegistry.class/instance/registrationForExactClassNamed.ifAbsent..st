querying
registrationForExactClassNamed: aClassName ifAbsent: absentBlock
  self configurationRegistry
    at: aClassName
    ifPresent: [ :registration | ^ registration ].
  self baselineRegistry
    at: aClassName
    ifPresent: [ :registration | ^ registration ].
  ^ absentBlock value