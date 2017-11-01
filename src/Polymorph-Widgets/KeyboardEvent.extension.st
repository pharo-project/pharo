Extension { #name : #KeyboardEvent }

{ #category : #'*Polymorph-Widgets' }
KeyboardEvent >> isWindowNavigation [

	^ TaskListMorph isNavigationEvent: self.
]
