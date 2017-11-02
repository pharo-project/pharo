Extension { #name : #ChangeRecord }

{ #category : #'*Monticello' }
ChangeRecord >> asMCMethodDefinition [
	"Creates a MCMethodDefinition from the receiver when this was created for a method (type=#method)"
	
	self type == #method ifFalse: [ ^nil ]. "This case shouldn't happen"
	
	^ MCMethodDefinition 
		className: class
		classIsMeta: meta
		selector: self methodSelector
		category: category
		timeStamp: stamp
		source: self string
]
