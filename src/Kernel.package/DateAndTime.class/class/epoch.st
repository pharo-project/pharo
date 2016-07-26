instance creation queries
epoch
  "Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

  ^ (self julianDayNumber: SqueakEpoch) offset: 0.
  