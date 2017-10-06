asserting
should: aBlock notTakeMoreThanMilliseconds: anInteger
    "For compatibility with other Smalltalks"

   self should: aBlock notTakeMoreThan: (Duration milliSeconds: anInteger).