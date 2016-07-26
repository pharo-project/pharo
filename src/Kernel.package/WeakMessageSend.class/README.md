Instances of WeakMessageSend encapsulate message sends to objects, like MessageSend. Unlike MessageSend it is not necessarily a valid mesage.  A request to value only results in a send if infact it is valid. 

See MessageSendComments also. WeakMessageSend is used primarily for event regristration. 

Unlike MessageSend WeakMessageSend stoes receiver (object receiving the message send) as a the first and only element of its array as opposed to a named ivar.
But like MessageSend, it does have
 selector		Symbol -- message selector
 arguments		Array -- bound arguments
and it also has
 shouldBeNil		Boolean --  used to ensure array of arguments is not all nils