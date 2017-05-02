instance variables
addSlot: aClassSlot

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	^self theNonMetaClass addClassSlot: aClassSlot