printing
printOn: aStream
    self print: self normalVersion prefix: nil on: aStream.
    self print: self preReleaseVersion prefix: $- on: aStream.
    self print: self buildVersion prefix: $+ on: aStream