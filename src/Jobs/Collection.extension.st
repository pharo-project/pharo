Extension { #name : #Collection }

{ #category : #'*Jobs' }
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

{ #category : #'*Jobs' }
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

	| size labelBlock count oldLabel lastUpdate |
	self isEmpty ifTrue: [ ^ self ].
	oldLabel := nil.
	count := lastUpdate := 0.
	size := self size.
	'' displayProgressFrom: 0 to: size during: [:bar |
		labelBlock := aStringOrBlock isString
			ifTrue: [
				bar label: aStringOrBlock.
				[ :dummyItem | aStringOrBlock] ]
			ifFalse: [ aStringOrBlock ].

		self do: [ :each | | newLabel |
			"Special handling for first and last element"
			(count = 0 or: [ count + 1 = size or: [(Time millisecondsSince: lastUpdate) >= msecs]]) 
				ifTrue: [ 
					bar current: count.
					oldLabel = (newLabel := (labelBlock cull: each) ifNil: [oldLabel]) 
						ifFalse: [
							bar label: newLabel.
							oldLabel := newLabel ].
				lastUpdate := Time millisecondClockValue ].
			aBlock value: each.
			count := count + 1 ] ]
]
