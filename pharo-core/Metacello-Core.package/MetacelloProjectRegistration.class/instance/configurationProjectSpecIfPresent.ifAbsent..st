accessing
configurationProjectSpecIfPresent: presentBlock ifAbsent: absentBlock
    ^ configurationProjectSpec ifNotNil: [ presentBlock cull: configurationProjectSpec ]  ifNil: absentBlock