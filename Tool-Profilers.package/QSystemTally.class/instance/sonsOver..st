report
sonsOver: threshold
	"Answer the sons with tallys over the given threshold"
	"threshold is a number "
	receivers ifNil: [ ^ #() ].
	^ receivers asArray select: [ :son | son tally > threshold ].