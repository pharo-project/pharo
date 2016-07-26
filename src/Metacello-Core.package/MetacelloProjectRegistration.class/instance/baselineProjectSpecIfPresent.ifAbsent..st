accessing
baselineProjectSpecIfPresent: presentBlock ifAbsent: absentBlock
     ^ baselineProjectSpec ifNotNil: [ presentBlock cull: baselineProjectSpec ]  ifNil: absentBlock