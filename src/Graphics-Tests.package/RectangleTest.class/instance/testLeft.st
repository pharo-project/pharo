tests - accessing
testLeft
|rect|
rect:=(0 @ 0 corner: 20 @ 20) left:10.
self assert: rect left = 10.