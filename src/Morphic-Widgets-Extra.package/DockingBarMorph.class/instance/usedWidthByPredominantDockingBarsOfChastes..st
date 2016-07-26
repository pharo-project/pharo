private - accessing
usedWidthByPredominantDockingBarsOfChastes: predominantChastes 
	"Private - convenience"
	^(self predominantDockingBarsOfChastes: predominantChastes)
		ifEmpty: [0]
		ifNotEmpty: [:predominants | (predominants collect: [:each | each width]) sum]