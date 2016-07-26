scripting
metacelloRegistrationHash
    ^ ((self map values sort: [ :a :b | a description <= b description ]) collect: [ :each | each description ]) hash