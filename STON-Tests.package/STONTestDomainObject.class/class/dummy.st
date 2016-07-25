instance creation
dummy
	^ self new
		integer: 999999 atRandom;
		float: 999 atRandom / Float pi;
		boolean: #(true false) atRandom;
		bytes: (ByteArray streamContents: [ :out | 32 timesRepeat: [ out nextPut: 255 atRandom ] ]);
		description: (String streamContents: [ :out | 16 atRandom timesRepeat: [ out nextPutAll: 'Blah' ] ]);
		color: #(#red #green #blue) atRandom;
		tags: (Array 
			with: #(#one #two #three) atRandom
 			with: #(#alpha #beta #gamma) atRandom 
			with: #(#low #medium #high) atRandom);
		yourself
