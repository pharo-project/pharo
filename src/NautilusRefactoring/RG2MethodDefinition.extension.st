Extension { #name : #RG2MethodDefinition }

{ #category : #'*NautilusRefactoring' }
RG2MethodDefinition >> basicAsMCMethodDefinition [
   "Creates a MCMethodDefinition from the receiver"
   
	^ self isActive 
		ifTrue: [ self asMCMethodDefinitionFromActiveDefinition ]
		ifFalse: [
		   ^ MCMethodDefinition
				className: self theNonMetaParentName
		 	   	classIsMeta: self isMetaSide
				selector: self selector
				category: self protocol
				timeStamp: self stamp
				source: self sourceCode ]
]

{ #category : #'*NautilusRefactoring' }
RG2MethodDefinition >> nameToDisplay [

	^ self printString
]
