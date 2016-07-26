packing
unpackDirectory: aFileReference
	"self default unpackDirectory: './gt/events/' asFileReference"
	^ aFileReference asFileReference allFiles 
			collect: [ :eachFile | 
				(eachFile basename = '.DS_Store') 
					ifTrue: [ nil ]
					ifFalse: [ self unpackFile: eachFile ] ]
			thenSelect: #notNil