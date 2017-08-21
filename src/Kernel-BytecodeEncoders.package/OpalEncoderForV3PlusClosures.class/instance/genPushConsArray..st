bytecode generation
genPushConsArray: size
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'numElements' index: size range: 0 to: 127].
	"138 	10001010 1kkkkkkk 	Push (Array new: kkkkkkk)"
	stream
		nextPut: 138;
		nextPut: size + 128