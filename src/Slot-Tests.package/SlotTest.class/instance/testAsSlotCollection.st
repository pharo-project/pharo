tests
testAsSlotCollection
	
 	self assert: 'a b' asSlotCollection = {#a => InstanceVariableSlot. #b => InstanceVariableSlot}.
	
	self assert: ('a _a a_a b b_' asSlotCollection collect: [ :i | i name ]) = #(#a #_a #a_a #b #b_) .
	
	self assert: ('1' asSlotCollection collect: [ :i | i name ]) = #(#v1).
	
	self assert: ('ivarA ivarB
 
ivarC
i%varD iv2arF



iva|rG
ivarH ivarE
' asSlotCollection collect: #name) = #(#ivarA #ivarB #ivarC #ivarD #iv2arF #ivarG #ivarH #ivarE)