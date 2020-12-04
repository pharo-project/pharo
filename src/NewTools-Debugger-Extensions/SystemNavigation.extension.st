Extension { #name : #SystemNavigation }

{ #category : #'*NewTools-Debugger-Extensions' }
SystemNavigation >> allHalts [

	^ self allBehaviors
		flatCollect:
			[ :class | class methods select: [ :method | method containsHalt ] ]
]
