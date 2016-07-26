This class models the context in which a set of transmissions take place. In essence, it records all ports that were reached after an outside event. 
 
That is necessary for ensuring that transmissions do not get propagated forever. When the first transmission is triggered, a context is created and this context will then store all ports that any subsequent transmission touches. This info is used to break possible cycles

Instance Variables
	ports:		Collection of Ports