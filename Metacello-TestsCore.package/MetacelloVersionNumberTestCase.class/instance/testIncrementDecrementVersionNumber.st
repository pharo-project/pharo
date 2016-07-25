tests version increment/decrement
testIncrementDecrementVersionNumber
  | version |
  super testIncrementDecrementVersionNumber.
  version := '1' asMetacelloVersionNumber.
  self assert: version incrementMinorVersionNumber asString = '2'.
  self assert: version decrementMinorVersionNumber asString = '1'.
  version := '1.0' asMetacelloVersionNumber.
  self assert: version incrementMinorVersionNumber asString = '1.1'.
  self assert: version decrementMinorVersionNumber asString = '1.0'.
  version := '1.0.0' asMetacelloVersionNumber.
  self assert: version incrementMinorVersionNumber asString = '1.0.1'.
  self assert: version decrementMinorVersionNumber asString = '1.0.0'.
  version := '1.0.0.0' asMetacelloVersionNumber.
  self assert: version incrementMinorVersionNumber asString = '1.0.0.1'.
  self assert: version decrementMinorVersionNumber asString = '1.0.0.0'