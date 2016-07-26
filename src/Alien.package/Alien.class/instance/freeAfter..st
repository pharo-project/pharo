accessing
freeAfter: aBlock
	"The receiver must be a pointer to a previously allocated external C heap memory block. Invoke aBlock passing the receiver as the argument, then free the receiver. Allows automatic allocation and freeing of C artifacts using patters such as
	(Alien newCString: 'foo') freeAfter: [:cstring | ...use cstring here...]."
	
	^[aBlock value: self] ensure: [self free]