Extension { #name : #Object }

{ #category : #'*GT-Spotter' }
Object >> asSpotterCandidateLink [

	^ GTSpotterCandidateLink value: self
]

{ #category : #'*GT-Spotter' }
Object >> asSpotterProcessorLink [

	^ GTSpotterProcessorLink value: self
]

{ #category : #'*GT-Spotter' }
Object >> gtFilter [
	^ GTFilterBlock gtFilter
		
]

{ #category : #'*GT-Spotter' }
Object >> gtIterator [
	^ Error signal: 'This is not a spotter iterator'
]

{ #category : #'*GT-Spotter' }
Object >> gtListFilter [
	^ GTFilterSubstring new
]

{ #category : #'*GT-Spotter' }
Object >> hasDynamicItems [
	^ false
]

{ #category : #'*GT-Spotter' }
Object >> spotterActDefault [
	^ self inspect
]

{ #category : #'*GT-Spotter' }
Object >> spotterActDefault: aStep [
	aStep exit.
	self spotterActDefault
]

{ #category : #'*GT-Spotter' }
Object >> spotterPreviewIn: aComposite inContext: aSpotter [
	| pragmas |
	pragmas := Pragma
		allNamed: #spotterPreview:
		from: self class
		to: Object
		sortedUsing: [ :x :y | (x argumentAt: 1) < (y argumentAt: 1) ].
	"We iterate through all pragmas and execute the corresponding methods
	to ensure that all possible previews are taken into account.
	Even though at this moment we only display one preview, we still have to go through
	all pragmas because it can happen that the method has a guarding if and thus, the actual
	preview would not be triggered."
	pragmas
		do: [ :eachPragma | 
			(eachPragma methodSelector findTokens: $:) size = 1
				ifTrue: [ self perform: eachPragma methodSelector with: aComposite ].
			(eachPragma methodSelector findTokens: $:) size = 2
				ifTrue: [ self perform: eachPragma selector with: aComposite with: aSpotter ].
			aComposite hasSubbricks
				ifTrue: [ ^ self ] ]
]

{ #category : #'*GT-Spotter' }
Object >> spotterPreviewRawIn: aComposite [
	<spotterPreview: 40>

	aComposite table
		title: [ self gtDisplayString ];
		display: [ :anObject | (anObject gtInspectorVariableValuePairs asSortedCollection asOrderedCollection yourself) addFirst: 'self' -> anObject; yourself ];
		column: 'Variable'
			evaluated: [:assoc |	GTObjectPrinter asNonTruncatedTextFrom: assoc key ]
			styled: [ :aBrick | aBrick textPosition: #rightCenter; margin: #(0 15 0 0); textColor: aComposite themer textLightColor; height: 30 ];
		column: 'Value'
			evaluated: [:assoc | GTObjectPrinter asTruncatedTextFrom: assoc value ]
			styled: [ :aBrick | aBrick margin: #(0 0 0 15); height: 30 ];
		entity: self.
		
	self flag: 'move "entity: self" somewhere else, maybe'
]

{ #category : #'*GT-Spotter' }
Object >> spotterProcessorsFor: aSpotterStep [
	"This is a utility method that collects all extensions (processors) for the current object.
	By default, it simply looks for the #spotterOrder: pragma.
	The step can decice whether or not an extension should be enabled.
	The step can also configure each extension (e.g. override any property)"
	
	^ (((Pragma 
		allNamed: #spotterOrder:
		from: self class 
		to: Object)
			select: [:pragma | aSpotterStep processorsFilter shouldEnableProcessorsCreateBy: pragma method])
			sort: [ :first :second | (first argumentAt: 1) < (second argumentAt: 1) ])
			do: [ :pragma ||currentSize|
				currentSize := aSpotterStep processors size.
				self spotterProcessorsFor: aSpotterStep pragma: pragma.
				aSpotterStep "configure the new processors if required"
					configureProcessorsStartingAt: currentSize+1 
					with: (aSpotterStep processorsFilter configurationBlockForProcessorsCreatedBy: pragma method)]
]

{ #category : #'*GT-Spotter' }
Object >> spotterProcessorsFor: aSpotterStep pragma: aPragma [
	" if there is a problem with the definition of one or more processors, let the other processors continue to work "

	[ | processorsSize addedProcessorsSize |
	"To allow nice arrangement of processor within UI we need to assign to
		each processor its spotter order. Because it is possible
		to define multiple (or none) processors using pragma
		we need to detect how many new processors were added"
	processorsSize := aSpotterStep processors size.
	self perform: aPragma methodSelector with: aSpotterStep.
	addedProcessorsSize := aSpotterStep processors size - processorsSize.
	"assign spotter order for newly added ones"
	(aSpotterStep processors last: addedProcessorsSize)
		do: [ :processor | processor order: (aPragma argumentAt: 1) ] ]
		on: Error
		do: [ :exception | 
			aSpotterStep exceptionHandler
				handleException: exception
				do: [ ('[Spotter] Exception while collecting processors for <'
						, self class name , '>: ' , exception asString) logCr ] ]
]
