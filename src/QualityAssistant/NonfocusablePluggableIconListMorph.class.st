"
Same as PluggableIconListMorph list but does not accept focus 
"
Class {
	#name : #NonfocusablePluggableIconListMorph,
	#superclass : #PluggableIconListMorph,
	#category : 'QualityAssistant'
}

{ #category : #display }
NonfocusablePluggableIconListMorph >> itemMorphFor: anObject index: anIndex [
	
	^ (super itemMorphFor: anObject index: anIndex)
		hResizing: #spaceFill;
		yourself
]

{ #category : #'event handling' }
NonfocusablePluggableIconListMorph >> takesKeyboardFocus [
	^ false 
]
