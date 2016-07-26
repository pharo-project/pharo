private
compareEqualTo: aMetacelloVersionNumber
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    (self compareEqualTo: self normalVersion other: aMetacelloVersionNumber normalVersion)
        ifFalse: [ ^ false ].
    (self compareEqualTo: self preReleaseVersion other: aMetacelloVersionNumber preReleaseVersion)
        ifFalse: [ ^ false ].
    ^ self compareEqualTo: self buildVersion other: aMetacelloVersionNumber buildVersion