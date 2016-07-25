keyboard managing
keyStroke: anEvent
	| char args |

	char := anEvent keyValue asCharacter. 

	(self arrowEvent: anEvent key: char)
		ifTrue: [ ^ true ].

	char = Character cr
		ifTrue: [ 
			self selectSearchedElement. 
			^ true ].
		
	anEvent anyModifierKeyPressed 
		ifFalse: [
			self basicKeyPressed: char.
			^ true].

	self keystrokeActionSelector 
		ifNil: [ ^ false].
		
	args := self keystrokeActionSelector numArgs.
	(args = 0 or: [args > 2]) 
		ifTrue: [^ self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol']
		ifFalse: [
			^ args = 1 
				ifTrue: [
					self listModel 
						perform: self keystrokeActionSelector 
						with: anEvent]
				ifFalse: [
					self listModel 
						perform: self keystrokeActionSelector
						with: anEvent 
						with: self]]