tests - accessing
testBottom
|rect|
rect:=(0 @ 0 corner: 20 @ 20) bottom:10.
self assert: rect bottom = 10.