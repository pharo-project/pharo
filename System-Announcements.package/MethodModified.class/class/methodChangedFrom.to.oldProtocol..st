instance creation
methodChangedFrom: oldMethod to: newMethod oldProtocol: oldProtocol 
	^ self new
		method: newMethod;
		oldMethod: oldMethod;
		oldProtocol: oldProtocol;
		yourself