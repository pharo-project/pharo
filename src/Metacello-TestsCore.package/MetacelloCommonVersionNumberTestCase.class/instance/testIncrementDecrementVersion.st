tests version increment/decrement
testIncrementDecrementVersion
  | version |
  version := self defaultVersion1.
  self assert: version incrementMajorVersion asString = '2.0.0'.
  self assert: version decrementMajorVersion asString = '1.0.0'.
  self assert: version incrementMinorVersion asString = '1.1.0'.
  self assert: version decrementMinorVersion asString = '1.0.0'.
  self assert: version incrementPatchVersion asString = '1.0.1'.
  self assert: version decrementPatchVersion asString = '1.0.0'