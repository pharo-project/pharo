tests - accessing
testRight
|rect|
rect:=(0 @ 0 corner: 20 @ 20) right:10.
self assert: rect right = 10.