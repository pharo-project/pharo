inheriting
installValue: anObject intoForked: newProcess from: ownerProcess
"
	In subclasses you can override this method to implement specific variable inheritance logic
"
	newProcess psValueAt: index put: anObject