tests - testing
testTheta
      | result dir tan x y |
	self assert: ((0 @ 1) theta - 90.0 degreesToRadians) abs < 1e-15.
	self assert: ((0 @ -1) theta - 270.0 degreesToRadians) abs < 1e-15.
	" See code of old and new theta"
	x := 1.0 . y := -1.0.
	tan := y  / x .
	dir := tan arcTan.
      result :=  360.0 degreesToRadians + dir.
      self assert: ((x @ y) theta - result) abs < 1e-15.
	x := -1.0. "Don't reuse old results whenyou want numeric precision!"
	tan := y  / x .
	dir := tan arcTan.
      result :=  180.0 degreesToRadians + dir.
      self assert: ((x @ y) theta - result) abs < 1e-15.

	