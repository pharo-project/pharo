Class {
	#name : 'MCLoadClassCommand',
	#superclass : 'MCBrowserAbstractClassCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCLoadClassCommand class >> defaultName [

	^ 'Load class...'
]

{ #category : 'executing' }
MCLoadClassCommand >> execute [
	"Load the selected class"

	self packageClasses
		detect: [ :ea | ea className = self selectedClass ]
		ifFound: [ :packageClass | 
			packageClass load.
			self methodsForSelectedClass 
				do: [ :m | m load ]
				displayingProgress: 'Loading definitions...' ]
]

{ #category : 'executing' }
MCLoadClassCommand >> methodsForSelectedClass [

	^ context methodsForSelectedClass
]

{ #category : 'executing' }
MCLoadClassCommand >> packageClasses [

	^ context packageClasses
]
