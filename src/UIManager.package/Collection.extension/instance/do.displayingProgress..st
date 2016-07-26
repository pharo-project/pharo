*UIManager
do: aBlock displayingProgress: aStringOrBlock
	"Enumerate aBlock displaying progress information. 
	If the argument is a string, use a static label for the process. 
	If the argument is a block, evaluate it with the element to retrieve the label.
		Smalltalk allClasses 
			do:[:aClass| (Delay forMilliseconds: 1) wait]
			displayingProgress: 'Processing...'.
		Smalltalk allClasses 
			do:[:aClass| (Delay forMilliseconds: 1) wait]
			displayingProgress:[:aClass| 'Processing ', aClass name].
	"
	^self do: aBlock displayingProgress: aStringOrBlock every: 20