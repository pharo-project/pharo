private
convertIndexToValue: anIndex with: numberOfPagers
	"private"
	numberOfPagers = self actualNumberOfVisiblePages
		ifTrue: [ ^ 0 ]
		ifFalse: [ ^ (anIndex - 1) / (numberOfPagers - self actualNumberOfVisiblePages ) ]