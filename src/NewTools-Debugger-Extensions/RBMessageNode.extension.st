Extension { #name : #RBMessageNode }

{ #category : #'*NewTools-Debugger-Extensions' }
RBMessageNode >> isHaltNode [

	^ (#( #halt #halt: #haltIf: #haltOnce #haltIfNil #haltOnCount: ) 
		   includes: self selector) or: [ 
		  self receiver isVariable and: [ 
			  self receiver variable value = Halt ] ]
]
