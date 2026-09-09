Extension { #name : 'OCMessageNode' }

{ #category : '*Debugging-Utils' }
OCMessageNode >> isHaltNode [

	^ (#( #halt #halt: #haltIf: #haltOnce #haltIfNil #haltOnCount: ) includes: self selector) or: [
		  self receiver isGlobalVariable and: [ self receiver variable value = Halt and: [ (Halt class selectorsInProtocol: #halting) includes: self selector ] ] ]
]
