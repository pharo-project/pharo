Extension { #name : #TabMorph }

{ #category : #'*Calypso-Browser' }
TabMorph >> markDirty: aBoolean [
	| currentLabel |
	
	"we want escape getting label from actual model because it simplified things 
	and in remote scenario it will not make any request to server"
	currentLabel := self label morph contents.
	
	aBoolean 
		ifTrue: [ 
			self label: '*', currentLabel ]
		ifFalse: [ 
			(currentLabel beginsWith: '*') 
				ifTrue: [ self label: currentLabel allButFirst ] ]
]
