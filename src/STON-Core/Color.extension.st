Extension { #name : #Color }

{ #category : #'*ston-core' }
Color class >> fromSton: stonReader [
	| representation |
	representation := stonReader parseMapOrListRepresentation.
	^ representation isArray
		ifTrue: [ 
			self named: representation first ]
		ifFalse: [ 
			(representation includesKey: #rgb)
				ifTrue: [ 
					self basicNew setRGB: (representation at: #rgb); setAlpha: 1.0 "backwards compatibility" ]
				ifFalse: [ 
					self 
						r: (representation at: #red) 
						g: (representation at: #green) 
						b: (representation at: #blue) 
						alpha: (representation at: #alpha) ] ]
]

{ #category : #'*ston-core' }
Color >> stonContainSubObjects [
	^ false
]

{ #category : #'*ston-core' }
Color >> stonOn: stonWriter [
	| name |
	(self isTranslucent or: [ (name := self name) = #unnamed ])
		ifTrue: [ 
			stonWriter writeObject: self streamMap: [ :map |
				#(red green blue alpha) do: [ :each | 
					map at: each put: ((self perform: each) roundTo: 0.001) ] ] ]
		ifFalse: [ 
			stonWriter writeObject: self listSingleton: name ]
]
