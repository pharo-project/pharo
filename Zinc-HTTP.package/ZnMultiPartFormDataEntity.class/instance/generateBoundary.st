private
generateBoundary
	^ String streamContents: [ :stream | | letters |
			stream nextPutAll: 'Boundary-Zn-'.
			letters := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
			8 timesRepeat: [ stream nextPut: letters atRandom ] ]
	