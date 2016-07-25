querying
packageNamed: aString forLoad: ignored forMap: map ifAbsent: absentBlock
  "at the end of the day, I don't the forLoad arg is important ... 
   it just ends up triggering an error ... when during a real load, you would 
   not get error. After a decent period of mourning we should just scrub this 
   arg from system"

  | importSpec |
  ^ map
    at: aString
    ifAbsent: [ 
      (self importArray notNil or: [ self import notNil ])
        ifTrue: [ 
          "expect the 'missing' name to be satisfied within context of imported project"
          importArray
            ifNotNil: [ 
              importArray
                do: [ :assoc | 
                  ((assoc value includes: aString) and: [ map includesKey: assoc key ])
                    ifTrue: [ 
                      importSpec := (map at: assoc key)
                        mergeImportLoads: {aString};
                        yourself ] ].
              importSpec ifNotNil: [ ^ importSpec ] ].
          (importSpec isNil and: [ self import notNil ])
            ifTrue: [ 
              ^ (map at: self import ifAbsent: absentBlock)
                mergeImportLoads: {aString};
                yourself ] ].
      (aString = 'default' or: [ aString = 'ALL' ])
        ifTrue: [ 
          self project groupSpec
            name: aString;
            includes: self packageNames;
            yourself ]
        ifFalse: [ absentBlock value ] ]