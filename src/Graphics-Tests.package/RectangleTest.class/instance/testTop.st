tests - accessing
testTop
|rect|
rect:=(0 @ 0 corner: 20 @ 20) top:10.
self assert: rect top = 10.