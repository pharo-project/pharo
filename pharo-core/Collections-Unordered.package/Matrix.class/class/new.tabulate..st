instance creation
new: dimension tabulate: aTwoArgumentBlock
	"Answer a dimension*dimension matrix where it at: i at: j is aBlock value: i value: j."
	^ self rows: dimension columns: dimension tabulate: aTwoArgumentBlock