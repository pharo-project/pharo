Extension { #name : 'Collection' }

{ #category : '*Jobs' }
Collection >> do: aBlock displayingProgress: aStringOrBlock [
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
]

{ #category : '*Jobs' }
Collection >> do: aBlock displayingProgress: aStringOrBlock every: msecs [
	"Enumerate aBlock displaying progress information.
 	 If the argument is a string, use a static label for the process.
	 If the argument is a block, evaluate it with the element to retrieve the label.
 	 The msecs argument ensures that updates happen at most every msecs.
       Example:
               Smalltalk allClasses
                       do:[:aClass| (Delay forMilliseconds: 1) wait]
                       displayingProgress:[:aClass| 'Processing ', aClass name]
                       every: 0."

	| size labelBlock oldLabel lastUpdate |
	self isEmpty ifTrue: [ ^ self ].
	oldLabel := nil.
	lastUpdate := 0.
	size := self size.
	
	[ : job |	
		| newLabel |
		job max: size.
		self doWithIndex: [ : each : index |
			"Handle String or Block progress label"
			labelBlock := aStringOrBlock isString
				ifTrue: [
					job title: aStringOrBlock.
					[ :dummyItem | aStringOrBlock] ]
				ifFalse: [ aStringOrBlock ].

			(index = 1 or: [ index + 1 = size or: [(Time millisecondsSince: lastUpdate) >= msecs]]) 
				ifTrue: [ 
					job currentValue: index.
					oldLabel = (newLabel := (labelBlock cull: each) ifNil: [oldLabel]) 
						ifFalse: [
							job title: newLabel.
							oldLabel := newLabel ].
				lastUpdate := Time millisecondClockValue ].
			aBlock value: each ] 
	] asJob run
	
]
