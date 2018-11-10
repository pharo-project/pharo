"
I provide compatibility of ClyMethodEditor with suggestions menu
"
Class {
	#name : #ClySugsMethodContext,
	#superclass : #SugsAbstractContext,
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #selection }
ClySugsMethodContext >> contentSelection [
	^self sourceTextArea textArea selection
]

{ #category : #selection }
ClySugsMethodContext >> selectedClass [
	^model methodClass
]

{ #category : #selection }
ClySugsMethodContext >> selectedInterval [

	^self sourceTextArea selectionInterval
]

{ #category : #selection }
ClySugsMethodContext >> selectedMethod [
	^model editingMethod
]

{ #category : #selection }
ClySugsMethodContext >> selectionInterval [
	^self selectedInterval 
]

{ #category : #accessing }
ClySugsMethodContext >> sourceTextArea [
	^ model textMorph
]
