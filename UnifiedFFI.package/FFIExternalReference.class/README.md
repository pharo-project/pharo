I represent a reference to some variable (a pointer, a memory space, an obscure structure) living in the C heap. 
I'm abstract, my children keep the real references.

A referenced type is  some variable that lives in the C heap and is seen in Pharo as an external  reference (an ==ExternalAddress==) .