Extension { #name : #ClassDescription }

{ #category : #'*Slot-Core' }
ClassDescription >> superclass: aSuperclass layout: aLayout [
	layout := aLayout. 
	
	self
		superclass: aSuperclass
		methodDictionary: self emptyMethodDictionary
		format: aLayout format.
]

{ #category : #'*Slot-Core' }
ClassDescription >> superclass: aSuperclass withLayoutType: layoutType slots: slotArray [
	| superLayout newScope newLayout |
	superLayout := aSuperclass
						ifNil: [ EmptyLayout instance ]
						ifNotNil: [ aSuperclass classLayout ].
	newScope := superLayout slotScope extend: slotArray.
	newLayout := layoutType extending: superLayout scope: newScope host: self.
	newLayout checkSanity.
	self
		superclass: aSuperclass
		layout: newLayout
]
