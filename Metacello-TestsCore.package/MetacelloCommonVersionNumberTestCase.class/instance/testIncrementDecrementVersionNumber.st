tests version increment/decrement
testIncrementDecrementVersionNumber
  | version |
  version := self defaultVersion1.
  self assert: version incrementMinorVersionNumber asString = '1.0.1'.
  self assert: version decrementMinorVersionNumber asString = '1.0.0'