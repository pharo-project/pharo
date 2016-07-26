tests - squeezing
testSqueezingOutNumbers
	"test that SqNumberParser squeezeNumberOutOfString finds numbers."
	
	self assert: '123blabla' squeezeOutNumber equals: 123.
	self assert: 'blabla123' squeezeOutNumber equals: 123.
	self assert: 'blabla12blabla' squeezeOutNumber equals: 12.
	self assert: ('12.3bla' squeezeOutNumber -12.3 ) abs < 0.0001.
	self assert: '.1' squeezeOutNumber > 0.
	
	self assert: 'blabla1230' squeezeOutNumber equals: 1230.