removing
removeKey: key 
 
 	^ self removeKey: key ifAbsent: [self errorKeyNotFound]