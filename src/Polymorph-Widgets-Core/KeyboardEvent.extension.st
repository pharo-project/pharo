Extension { #name : #KeyboardEvent }

{ #category : #'*Polymorph-Widgets-Core' }
KeyboardEvent >> isWindowNavigation [

	^ TaskListMorph isNavigationEvent: self.
]
