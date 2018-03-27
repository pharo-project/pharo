Extension { #name : #Dictionary }

{ #category : #'*Slot-Core' }
Dictionary >> declareVariable: newGlobal from: aDictionary [ 
	"Add aGlobal to the receiver. If key already exists, do nothing. If aDictionary 
	includes key, then remove it from aDictionary and use its association as 
	the element of the receiver."

	| globalName |
	globalName :=  newGlobal key.
	self associationAt: globalName ifPresent:  [:existingGlobal |
		"need to take care to migrate existing variables to new global if class if different"
		(existingGlobal class == newGlobal class) 
			ifTrue: [^self].
		newGlobal value: existingGlobal value.
		self removeKey: globalName.
		self add: newGlobal.
		].
	(aDictionary includesKey: globalName)
		ifTrue:  [
			self add: ((aDictionary associationAt: globalName) primitiveChangeClassTo: ClassVariable new).
			aDictionary removeKey: globalName]
		ifFalse: [
			self add: newGlobal]
]
