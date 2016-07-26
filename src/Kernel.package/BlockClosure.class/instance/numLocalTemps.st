accessing
numLocalTemps
	"Answer the number of local temporaries for the receiver"
	^ self method encoderClass numLocalTempsForBlockAt: startpc in: outerContext method