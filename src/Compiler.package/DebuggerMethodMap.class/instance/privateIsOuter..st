private
privateIsOuter: anObject 
	^anObject last isArray and: [anObject last first == #outer]