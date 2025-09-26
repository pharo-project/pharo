Extension { #name : 'RGMethodDefinition' }

{ #category : '*Monticello-Model' }
RGMethodDefinition >> asMCMethodDefinition [
	"Creates a MCMethodDefinition from the receiver"

	^ self isActive
		  ifTrue: [ self asMCMethodDefinitionFromActiveDefinition ]
		  ifFalse: [
				  ^ MCMethodDefinition
					    className: self instanceSideParentName
					    classIsMeta: self isMeta
					    selector: self selector
					    category: self protocol
					    timeStamp: self stamp
					    source: self sourceCode ]
]

{ #category : '*Monticello-Model' }
RGMethodDefinition >> asMCMethodDefinitionFromActiveDefinition [
	"Creates a MCMethodDefinition from a source file assuming the current method isActive."

	| compiledMethod methodClass |
	compiledMethod := self compiledMethod.
	methodClass := compiledMethod methodClass.

	^ MCMethodDefinition
		  className: methodClass instanceSide name
		  classIsMeta: methodClass isMeta
		  selector: compiledMethod selector
		  category: compiledMethod protocolName
		  timeStamp: compiledMethod timeStamp
		  source: compiledMethod sourceCode
]

{ #category : '*Monticello-Model' }
RGMethodDefinition >> basicAsMCMethodDefinition [
   "Creates a MCMethodDefinition from the receiver"
   
	^ self isActive 
		ifTrue: [ self asMCMethodDefinitionFromActiveDefinition ]
		ifFalse: [
		   ^ MCMethodDefinition
				className: self instanceSideParentName
		 	   	classIsMeta: self isMeta
				selector: self selector
				category: self protocol
				timeStamp: self stamp
				source: self sourceCode ]
]
