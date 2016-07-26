bytecode generation
genPushNewArray: size
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'size' index: size range: 0 to: 127].
	"231		11100111	jkkkkkkk	Push (Array new: kkkkkkk) (j = 0)
									&	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)"
	stream
		nextPut: 231;
		nextPut: size