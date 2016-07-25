By John M McIntosh johnmci@smalltalkconsulting.com
This class was written to mange the external semaphore table. When I was writing a Socket test server I discovered various race conditions on the access to the externalSemaphore table. This new class uses class side methods to restrict access using two mutex semaphores, one for removal and one for additions to the table. It seemed cleaner to deligate the reponsibility here versus adding more code and another class variable to SystemDictionary 

Note that in Smalltalk recreateSpecialObjectsArray we still directly play with the table.

Henrik Sperre Johansen
The name is somewhat of a misnomer; the table can be used for any objects, not just semaphores.
That is its main usage though, so a split which deals with semaphores and other external objects differently 
(In the same underlying table) is not currently worth it.
Therefore, while in general not all users will care if the table is above a certain size, we still guard  against adding more objects than the limit above which external signals would be lost (on some VMs.)
