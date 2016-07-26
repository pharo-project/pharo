private
sortBlockTimestamp
	"Answer block to decide what order to display the directory entries."

	^ [ :x :y | |xIsDir|
			((xIsDir := x isDirectory) = y isDirectory) 
				ifTrue: [   x modificationTime  <= y modificationTime ]
				ifFalse: [
					"directories always precede files"
					xIsDir ]]